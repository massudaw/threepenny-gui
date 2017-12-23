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



joinBuffer = intercalate ";" . ($[])

-- | Event loop for a browser window.
-- Supports concurrent invocations of `runEval` and `callEval`.
eventLoop :: (Window -> IO (IO ())) ->  (Comm -> IO ())
eventLoop init comm = do
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
               , callEval       = (\ref -> call ref . CallEval. joinBuffer)
               , debug        = debug
               , timestamp    = atomically $ run Timestamp
               , onDisconnect = onDisconnect
               }

    -- The individual threads are as follows:
    --
    -- Read client messages and send them to the
    -- thread that handles events or the thread that handles FFI calls.
    let multiplexer = void $ untilJustM $ do
            atomically $ do
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
                    Quit -> fail "Foreign.JavaScript: Browser <-> Server Client Quit."

    let flushTimeout = forever $ do
            i <- atomically $ flushDirtyBuffer comm w
            threadDelay (i*1000)
    -- Send FFI calls to client and collect results
    let handleCalls = forever $ do
            (ref,msg) <- atomically $ do
                (ref, msg) <- readTQueue calls
                return (ref,msg)
            (do
              traverse (\msg-> atomically $ writeServer comm msg) (notEmptyMsg msg)
              return ())`E.catch` (\e -> putStrLn (show (e ::E.SomeException)))
            atomically $ do
                case ref of
                    Just ref -> do
                        result <- readTQueue results
                        putTMVar ref result
                    Nothing  -> return ()

    -- Receive events from client and handle them in order.
    let handleEvents = (do
            init w
            forever $ do
                e <- atomically $ do
                    readTQueue events
                handleEvent w e
                rebug
                )`E.catch` (\e -> print  (e :: SomeException))

    Foreign.withRemotePtr (wRoot w) $ \_ _ -> do    -- keep root alive
        printException $
          E.finally
              (foldr1 race_ [multiplexer, handleEvents, handleCalls,flushTimeout])
              (do
                putStrLn "Foreign.JavaScript: Browser window disconnected."
                m <- atomically $ tryTakeTMVar disconnect
                maybe (putStrLn "No disconnect event ") id m
                b <- atomically $ readTVar (commOpen comm)
                when b (commClose comm)
                  )

    return ()

-- | Execute an IO action, but also print any exceptions that it may throw.
-- (The exception is rethrown.)
printException :: IO a -> IO a
printException = E.handle $ \e -> do
    putStrLn $ show (e :: E.SomeException)
    E.throwIO e

-- | Repeat an action until it returns 'Just'. Similar to 'forever'.
untilJustM :: Monad m => m (Maybe a) -> m a
untilJustM m = m >>= \x -> case x of
    Nothing -> untilJustM m
    Just a  -> return a

{-----------------------------------------------------------------------------
    Exports, Imports and garbage collection
------------------------------------------------------------------------------}
-- | Turn a Haskell function into an event handler.
newHandler :: Window -> ([JSON.Value] -> IO ()) -> IO HsEvent
newHandler w@(Window{..}) handler = do
    coupon <- newCoupon wEventHandlers
    newJSPtr w coupon (handler . parseArgs) wEventHandlers
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
        Nothing -> do
          newJSPtr w coupon (JSPtr coupon) wJSObjects


flushDirtyBuffer :: Comm -> Window -> STM Int
flushDirtyBuffer comm w@Window{..} = do
    do
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



ifOpen comm stm = do
            r <- do
                b <- readTVar (commOpen comm)
                if b then fmap Right stm else return (Left ())
            case r of
                Right a -> return a
                Left  _ -> error "Foreign.JavaScript: Browser <-> Server communication broken."

