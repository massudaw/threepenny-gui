{-# LANGUAGE RecordWildCards #-}
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
    JavaScriptException(..),JSCode(..),ToJS(..), FromJS, JSAsync(..),JSFunction(..),emptyFunction, JSObject,
    FFI, ffi, runFunction, callFunction,
    NewJSObject, unsafeCreateJSObject,
    CallBufferMode(..), setCallBufferMode, flushCallBuffer,
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
import qualified Data.Foldable as F

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
  bufferRunEval w  (toCode f)


-- | Run a JavaScript function that creates a new object.
-- Return a corresponding 'JSObject' without waiting for the browser
-- to send a result.
--
-- WARNING: This function assumes that the supplied JavaScript code does,
-- in fact, create an object that is new.
unsafeCreateJSObject :: Window -> JSFunction NewJSObject -> IO JSObject
unsafeCreateJSObject w f = do
    g <- wrapImposeStablePtr w f
    bufferRunEval w (toCode g)
    marshalResult g w JSON.Null

-- | Call a JavaScript function and wait for the result.
callFunction :: Window -> JSFunction a -> IO a
callFunction w f = do
    ref <- newEmptyTMVarIO
    bufferCallEval w ref (fmap ("return " <> ) $ toCode f)
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

{-----------------------------------------------------------------------------
    Call Buffer
------------------------------------------------------------------------------}
-- | Set the call buffering mode for the given browser window.
setCallBufferMode :: Window -> CallBufferMode -> IO ()
setCallBufferMode w@Window{..} new = do
    flushCallBuffer w
    atomically $ writeTVar wCallBufferMode new

-- | Flush the call buffer,
-- i.e. send all outstanding JavaScript to the client in one single message.
flushCallEvalBuffer :: Window -> TMVar Result -> IO ()
flushCallEvalBuffer w@Window{..} ref = do
    code' <- atomically $ do
        code <- readTVar wCallBuffer
        writeTVar wCallBuffer return
        return code

    callEval ref (intercalate ";"  <$> code' [])
    return ()


flushCallBuffer :: Window -> IO ()
flushCallBuffer w@Window{..} = do
    code' <- atomically $ do
        code <- readTVar wCallBuffer
        writeTVar wCallBuffer return
        return code
    runEval (intercalate ";" <$> code' [])

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferCallEval :: Window -> TMVar Result -> IO String -> IO ()
bufferCallEval w@Window{..} ref icode = do
  action <- atomically $ do
        mode <- readTVar wCallBufferMode
        let buffer = do
                msg <- readTVar wCallBuffer
                writeTVar wCallBuffer (\i -> do
                  code <- icode
                  msg (code :i) )
                return Nothing

        case mode of
            BufferAll -> buffer
            BufferCall -> buffer
            i -> do
              return $ Just icode
  case action of
    Nothing -> flushCallEvalBuffer w ref
    Just i -> callEval ref i

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferRunEval :: Window -> IO String -> IO ()
bufferRunEval w@Window{..} icode = do
  action <- atomically $ do
        mode <- readTVar wCallBufferMode
        let buffer = do
                msg <- readTVar wCallBuffer
                writeTVar wCallBuffer (\i -> do
                  code <- icode
                  msg (code :i) )
                return Nothing
        case mode of
            BufferAll ->  buffer
            BufferRun -> buffer
            i-> do
              return $ Just icode
  case action of
    Nothing -> return ()
    Just i -> runEval i
