{-# LANGUAGE RecordWildCards, CPP #-}
{-# LANGUAGE RecursiveDo #-}
module Foreign.JavaScript.EventLoop (
    eventLoop,
    runEval, callEval, debug, onDisconnect,flushCallBufferSTM,
    newHandler, fromJSStablePtr,
    Result
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM   as STM
import           Control.DeepSeq                  (deepseq)
import           Control.Exception        as E
import           Control.Monad
import qualified Data.Aeson               as JSON
import qualified Data.ByteString.Char8    as BS
import           Data.IORef
import qualified Data.Text                as T
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified System.Mem

import Data.Time
import Control.Concurrent
import Foreign.RemotePtr        as Foreign
import Foreign.JavaScript.CallBuffer
import Foreign.JavaScript.Types
import Debug.Trace
import GHC.Conc
import GHC.Clock
import Data.Word

rebug :: IO ()
#ifdef REBUG
rebug = System.Mem.performGC
#else
rebug = return ()
#endif

{-----------------------------------------------------------------------------
    Event Loop
------------------------------------------------------------------------------}
-- | Handle a single event
handleEvent w@(Window{..}) (name, args) = do
    mhandler <- Foreign.lookup name wEventHandlers
    case mhandler of
        Nothing -> return ()
        Just f  -> withRemotePtr f (\_ f -> f args)



joinBuffer :: ([String]-> [String]) -> String
joinBuffer = intercalate ";" . ($[])

-- | Event loop for a browser window.
-- Supports concurrent invocations of `runEval` and `callEval`.
eventLoop :: (Window -> IO ())-> EventLoop
eventLoop init cookie server comm = do
    -- To support concurrent FFI calls, we make three threads.

    -- The thread `multiplexer` reads from the client and
    --   sorts the messages into the appropriate queue.
    events      <- newTQueueIO
    results     <- newTQueueIO :: IO (TQueue Result)
    -- The thread `handleCalls` executes FFI calls
    --    from the Haskell side in order.
    -- The corresponding queue records `TMVar`s in which to put the results.
    calls       <- newTQueueIO :: IO (TQueue (Maybe (TMVar Result), ServerMsg))
    -- The thread `handleEvents` handles client Events in order.


    -- We only want to make an FFI call when the connection browser<->server is open
    -- Otherwise, throw an exception.

    -- FFI calls are made by writing to the `calls` queue.
    let run msg =
          ifOpen comm $ writeTQueue calls (Nothing , msg)
        call ref  msg =
          ifOpen comm $ writeTQueue calls (Just ref, msg)
        debug    s =
          atomically $ ifOpen comm$ writeServer comm $ Debug s

    -- We also send a separate event when the client disconnects.
    disconnect <- newTVarIO $ return ()
    let onDisconnect m = atomically $ writeTVar disconnect m

    w0 <- newPartialWindow
    let w = w0 { runEval        = maybe (return ()) (run  . RunEval) . nonEmpty . joinBuffer
               , getServer    = server
               , requestInfo = cookie
               , callEval       = (\ref -> call ref . CallEval. joinBuffer)
               , debug        = debug
               , timestamp    = atomically $ run Timestamp
               , onDisconnect = onDisconnect
               }

    -- The individual threads are as follows:
    --
    -- Read client messages and send them to the
    -- thread that handles events or the thread that handles FFI calls.
    let multiplexer = void $ forever $ do
            v <- atomically $ do
              msg <- readClient comm
              case msg of
                  Event x y   -> do
                    writeTQueue events (x,y)
                    return Nothing
                  Result x  -> do
                    writeTQueue results (Right x)
                    return Nothing
                  Exception e -> do
                    writeTQueue results (Left  e)
                    return Nothing
                  Quit -> do
                    return (Just ())
            traverse (\_ -> do
              m <- atomically $ readTVar disconnect
              m
              b <- atomically $ readTVar (commOpen comm)
              when b (commClose comm)) v

    let flushTimeout = flushBuffers w  
    -- Send FFI calls to client and collect results
    let handleCalls = forever $ do
            ref <- atomically $ do
              (ref,msg) <- readTQueue calls
              traverse (writeServer comm) (notEmptyMsg msg)
              return ref
            case ref of
               Just i -> atomically $ do
                  result <- readTQueue results
                  putTMVar i result
               Nothing -> return ()

    -- Receive events from client and handle them in order.
    let handleEvents = (do
            init w
            forever $ do
                e <- atomically $ readTQueue events
                handleEvent w e
                rebug
                )

    Foreign.withRemotePtr (wRoot w) $ \_ _ -> do    -- keep root alive
        printException $
          E.finally
              (foldr1 race_ [multiplexer, handleEvents, handleCalls,flushTimeout])
              (do
                putStrLn "Foreign.JavaScript: Browser window disconnected."
                m <- atomically $ readTVar disconnect
                m
                b <- atomically $ readTVar (commOpen comm)
                when b (commClose comm))
    return ()

-- | Execute an IO action, but also print any exceptions that it may throw.
-- (The exception is rethrown.)
printException :: IO a -> IO a
printException = E.handle $ \e -> do
  putStrLn $ "Threepenny Exception: " ++ show (e :: E.SomeException)
  E.throwIO e


{-----------------------------------------------------------------------------
    Exports, Imports and garbage collection
------------------------------------------------------------------------------}
-- | Turn a Haskell function into an event handler.
newHandler :: Window -> ([JSON.Value] -> IO ()) -> IO HsEvent
newHandler w@(Window{..}) handler = do
    coupon <- newCoupon wEventHandlers
    newRemotePtr coupon (handler . parseArgs) wEventHandlers
    where
    fromSuccess (JSON.Success x) = x
    -- parse a genuine JavaScript array
    parseArgs x = fromSuccess (JSON.fromJSON x) :: [JSON.Value]
    -- parse a JavaScript arguments object
    -- parseArgs x = Map.elems (fromSuccess (JSON.fromJSON x) :: Map.Map String JSON.Value)

-- | Convert a stable pointer from JavaScript into a 'JSObject'.
fromJSStablePtr :: JSON.Value -> Window -> IO JSObject
fromJSStablePtr js w@(Window{..}) = do
    let JSON.Success coupon = JSON.fromJSON js
    mhs <- Foreign.lookup coupon wJSObjects
    case mhs of
        Just hs -> return hs
        Nothing -> newJSPtr w coupon (JSPtr coupon) wJSObjects


flushBuffers :: Window -> IO () 
flushBuffers w = forever $ do
    i <- atomically $ flushDirtyBuffer w 
    threadDelay (fromIntegral i `div` 1000) 

flushDirtyBuffer ::  Window -> STM Word64
flushDirtyBuffer w@Window{..} = do
      stats <- readTVar wCallBufferStats
      case stats of 
        Nothing -> retry 
        Just (ti,tl,ix) -> do
          tc  <- unsafeIOToSTM getMonotonicTimeNSec 
          let delta = tc - tl
              total = tl - ti 
          if delta > flushLimitMin || total > flushLimitMax 
            then do
              flushCallBufferSTM w
              writeTVar wCallBufferStats Nothing 
              return flushLimitMin
            else do
              return (flushLimitMin - delta)


flushLimitMin :: Word64 
flushLimitMin = 16 *1000* 1000

flushLimitMax :: Word64 
flushLimitMax = 100 * 1000 *1000


ifOpen :: Comm -> STM a -> STM a
ifOpen comm stm = do
  b <- readTVar (commOpen comm)
  if b then stm else error "Foreign.JavaScript: Browser <-> Server communication broken."

