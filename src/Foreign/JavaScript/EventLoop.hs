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
import           Control.Exception        as E
import           Control.Monad
import qualified Data.Aeson               as JSON
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
eventLoop init cookie comm = do
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
    disconnect <- newTMVarIO $ return ()
    let onDisconnect m = atomically $ takeTMVar disconnect >>= putTMVar disconnect . (>>m)

    w0 <- newPartialWindow
    let w = w0 { runEval        = maybe (return ()) (run  . RunEval) . nonEmpty . joinBuffer
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
              m <- atomically $ tryTakeTMVar disconnect
              maybe (putStrLn "No disconnect event ") id m
              b <- atomically $ readTVar (commOpen comm)
              when b (commClose comm)) v

    let flushTimeout = forever $ do
            i <- atomically $ flushDirtyBuffer comm w
            threadDelay (i*1000)
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
                m <- atomically $ tryTakeTMVar disconnect
                maybe (putStrLn "No disconnect event ") id m
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


flushDirtyBuffer :: Comm -> Window -> STM Int
flushDirtyBuffer comm w@Window{..} = do
      (ti,tl,ix) <- takeTMVar wCallBufferStats
      tc <- unsafeIOToSTM getCurrentTime
      let delta = round $ diffUTCTime tc tl *1000
          total = round $ diffUTCTime tl ti *1000
      if delta > flush_limit_min || total > flush_limit_max
        then do
          flushCallBufferSTM w
          return flush_limit_min
        else do
          putTMVar wCallBufferStats (ti,tl,ix)
          return (flush_limit_min - delta)


flush_limit_min :: Int
flush_limit_min = 16

flush_limit_max :: Int
flush_limit_max = 100


ifOpen :: Comm -> STM a -> STM a
ifOpen comm stm = do
  b <- readTVar (commOpen comm)
  if b then stm else error "Foreign.JavaScript: Browser <-> Server communication broken."

