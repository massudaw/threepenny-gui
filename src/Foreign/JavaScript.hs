{-# LANGUAGE TupleSections,RecordWildCards #-}
module Foreign.JavaScript (
    -- * Synopsis
    -- | A JavaScript foreign function interface (FFI).
    --
    -- This module implements a web server that communicates with
    -- a web browser and allows you to execute arbitrary JavaScript code on it.
    --
    -- Note: This module is used internally by the "Graphics.UI.Threepenny"
    -- library, but the types are /not/ compatible.
    -- Use "Foreign.JavaScript" only if you want to roll your own
    -- interface to the web browser.

    -- * Server
    serve, Config(..), defaultConfig,
    Window, root,

    -- * JavaScript FFI
    JavaScriptException(..),JSCode(..),ToJS(..), FromJS, JSAsync(..),JSFunction(..),emptyFunction, JSObject,toCode,
    FFI, ffi, runFunction, runFunctionDelayed, callFunction,
    NewJSObject, unsafeCreateJSObject,
    CallBufferMode(..), setCallBufferMode, flushCallBuffer, flushChildren, forceObject,
    IsHandler, exportHandler, onDisconnect,
    debug, timestamp,addFinalizer
    ) where

import           Control.Concurrent.STM       as STM
import           Control.Monad                           (unless)
import qualified Data.Aeson                   as JSON
import qualified Control.Exception as E
import           Foreign.JavaScript.EventLoop
import           Foreign.JavaScript.Marshal
import           Foreign.JavaScript.Server
import           Foreign.JavaScript.Types
import           Foreign.RemotePtr            as Foreign
import Data.List (any,intercalate)
import Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Foldable as F
import Control.Monad
import Data.Time
import Debug.Trace
{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
-- | Run a "Foreign.JavaScript" server.
serve
    :: Config               -- ^ Configuration options.
    -> (Window -> IO (IO ()))    -- ^ Initialization whenever a client connects.
    -> IO ()
serve config init = httpComm config $ eventLoop $ \w -> do
  i <- init w
  flushCallBuffer w   -- make sure that all `runEval` commands are executed
  return i

{-----------------------------------------------------------------------------
    JavaScript
------------------------------------------------------------------------------}
-- | Run a JavaScript function, but do not wait for a result.
--
-- NOTE: The JavaScript function is subject to buffering,
-- and may not be run immediately. See 'setCallBufferMode'.
runFunction :: Window -> JSFunction () -> IO ()
runFunction w f = do
  atomically. bufferRunEval w  =<< toCode f

{-----------------------------------------------------------------------------
    JavaScript
------------------------------------------------------------------------------}
-- | Run a JavaScript function, but do not wait for a result.
--
-- NOTE: The JavaScript function is subject to buffering,
-- and may not be run immediately. See 'setCallBufferMode'.
runFunctionDelayed :: Window -> JSObject -> JSFunction () -> IO ()
runFunctionDelayed w js f = do
  bufferRunEvalMethod w js =<< toCode f



-- | Run a JavaScript function that creates a new object.
-- Return a corresponding 'JSObject' without waiting for the browser
-- to send a result.
--
-- WARNING: This function assumes that the supplied JavaScript code does,
-- in fact, create an object that is new.
unsafeCreateJSObject :: Window -> JSFunction NewJSObject -> IO JSObject
unsafeCreateJSObject w f = do
    g <- wrapImposeStablePtr w f
    atomically . bufferRunEval w =<< toCode g
    o <- marshalResult g w JSON.Null
    cid  <- unprotectedGetCoupon o
    addFinalizer o (atomically $ modifyTVar (wCallBufferMap  w) (\(k,v) -> (Set.delete cid k ,v)))
    return o

-- | Call a JavaScript function and wait for the result.
callFunction :: Window -> JSFunction a -> IO a
callFunction w f = do
    ref <- newEmptyTMVarIO
    atomically . bufferCallEval w ref . ("return " <> ) =<<  toCode f
    resultJS <- atomically $ takeTMVar ref
    case resultJS of
        Left  e -> E.throwIO $ JavaScriptException e
        Right x -> marshalResult f w x

-- | Export a Haskell function as an event handler.
--
-- The result is a JavaScript @Function@ object that can be called
-- from JavaScript like a regular function. However,
-- the corresponding Haskell function will /not/ be run immediately,
-- rather it will be added to the event queue and processed
-- like an event. In other words, this the Haskell code is only called
-- asynchronously.
--
-- WARNING: The event handler will be garbage collected unless you
-- keep a reference to it /on the Haskell side/!
-- Registering it with a JavaScript function will generally /not/
-- keep it alive.
exportHandler :: IsHandler a => Window -> a -> IO JSObject
exportHandler w f = do
    g <- newHandler w (\args -> handle f w args >> flushCallBuffer w)
    h <- unsafeCreateJSObject w $
        ffi "Haskell.newEvent(%1,%2)" g (convertArguments f)
    Foreign.addReachable h g
    return h

flushCallBuffer :: Window -> IO ()
flushCallBuffer = atomically . flushCallBufferSTM

{-----------------------------------------------------------------------------
    Call Buffer
------------------------------------------------------------------------------}
-- | Set the call buffering mode for the given browser window.
setCallBufferMode :: Window -> CallBufferMode -> IO ()
setCallBufferMode w@Window{..} new = atomically $ do
    flushCallBufferSTM w
    writeTVar wCallBufferMode new

-- | Flush the call buffer,
-- i.e. send all outstanding JavaScript to the client in one single message.
flushCallEvalBuffer :: Window -> TMVar Result -> STM ()
flushCallEvalBuffer w@Window{..} ref = do
        code <- readTVar wCallBuffer
        writeTVar wCallBuffer  id
        callEval ref  code
        return ()




-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferCallEval :: Window -> TMVar Result -> String -> STM ()
bufferCallEval w m c = bufferCallEval' w m (snockBuffer c)

bufferCallEval' :: Window -> TMVar Result -> CallBuffer -> STM ()
bufferCallEval' w@Window{..} ref icode =  do
  action <- do
        mode <- readTVar wCallBufferMode
        let buffer = do
                modifyTVar wCallBuffer (appendBuffer icode)
                return Nothing

        case mode of
            BufferAll -> buffer
            BufferCall -> buffer
            i -> do
              return $ Just icode
  case action of
    Nothing -> flushCallEvalBuffer w ref
    Just i -> callEval ref i

forceObject :: Window -> JSObject -> IO ()
forceObject  w@Window{..} top = do
  root <- unprotectedGetCoupon top
  void $ atomically $ do
    action <- do
         (rendered,map) <- readTVar wCallBufferMap
         if Set.member root rendered
            then  return Nothing
            else
              case findBM root map of
                Just (s,m) ->  do
                  writeTVar wCallBufferMap (Set.union s rendered,deleteBM root map)
                  msg <- readTVar m
                  return $Just msg
                Nothing ->   do
                  writeTVar wCallBufferMap (Set.insert root rendered,map)
                  return Nothing
    traverse (bufferRunEval' w) action

flushChildren :: Window -> JSObject -> JSObject -> IO ()
flushChildren w@Window{..} top child = do
  root <- unprotectedGetCoupon top
  childs <- unprotectedGetCoupon child
  void $ atomically $ do
    mode <- readTVar wCallBufferMode
    let childFun c = do
            (rendered, map) <- readTVar wCallBufferMap
            if Set.member c rendered
              then return Nothing
              else
                case findBM c map of
                  Just (k,ref) ->  fmap (Just .(k,))$ readTVar ref
                  Nothing -> return Nothing
    action <- do
        (rendered,map) <- readTVar wCallBufferMap
        childActions <- childFun childs
        if Set.member root rendered
           then
             case childActions of
               Just (k,v) ->  do
                 writeTVar wCallBufferMap (Set.insert root (Set.union k rendered), deleteBM childs map)
                 return (Just v)
               i ->  do
                 writeTVar wCallBufferMap (Set.insert root  $ Set.insert childs $ rendered, map)
                 return Nothing
           else do
             let val = findBM root map
             case childActions of
               Just (kc,act) -> do
                 case val of
                   Just (k,v) ->  do
                     modifyTVar v  (flip appendBuffer act)
                     out <- readTVar v
                     writeTVar wCallBufferMap (rendered , moveAtBM k childs map)
                     return Nothing
                   Nothing ->  do
                     writeTVar wCallBufferMap (rendered , (\(k,v) -> if Set.member childs k then (Set.insert root k,v) else (k,v)) <$> map)
                     return Nothing
               Nothing -> do
                 case val of
                   Just (kr,_) -> do
                     writeTVar wCallBufferMap (rendered , (\(k,v) -> if Set.member root k then (Set.insert childs k,v) else (k,v)) <$>  map)
                     return Nothing
                   Nothing ->  do
                     var <- newTVar id
                     writeTVar wCallBufferMap (rendered , insertSBM (Set.fromList [root,childs]) var map)
                     return Nothing
    traverse (bufferRunEval' w ) action

appendBuffer xs ys = ys . xs
snockBuffer e =  (e:)

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferRunEvalMethod :: Window -> JSObject -> String -> IO ()
bufferRunEvalMethod w j s = bufferRunEvalMethod' w j (snockBuffer s)

bufferRunEvalMethod' :: Window -> JSObject -> CallBuffer -> IO ()
bufferRunEvalMethod'  w@Window{..} js icode = do
  coupon <- unprotectedGetCoupon js
  void $ atomically $ do
    mode <- readTVar wCallBufferMode
    action <- do
       (rendered,map )<- readTVar wCallBufferMap
       if Set.member coupon rendered
         then return (Just icode)
         else  do
            case findBM coupon map of
              Just (k,ref) -> do
                modifyTVar ref (appendBuffer icode)
              Nothing -> do
                ref <- newTVar icode
                writeTVar wCallBufferMap (rendered,insertBM coupon ref map)
            return Nothing
    traverse (bufferRunEval' w) action

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferRunEval w icode = bufferRunEval' w (snockBuffer icode)
bufferRunEval' :: Window -> CallBuffer -> STM ()
bufferRunEval' w@Window{..} icode = do
      mode <- readTVar wCallBufferMode
      let buffer = do
              modifyTVar wCallBuffer (appendBuffer icode)
              return Nothing
      o <- case mode of
            BufferAll ->  buffer
            BufferRun -> buffer
            i-> return $ Just icode
      traverse runEval o
      return ()
