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
    ToJS(..), FromJS, JSFunction, JSObject,
    FFI, ffi, runFunction, callFunction,
    NewJSObject, unsafeCreateJSObject,
    CallBufferMode(..), setCallBufferMode, flushCallBuffer,
    IsHandler, exportHandler, onDisconnect,
    debug, timestamp,
    ) where

import           Control.Concurrent.STM       as STM
import           Control.Monad                           (unless)
import qualified Data.Aeson                   as JSON
import           Foreign.JavaScript.EventLoop
import           Foreign.JavaScript.Marshal
import           Foreign.JavaScript.Server
import           Foreign.JavaScript.Types
import           Foreign.RemotePtr            as Foreign

{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
-- | Run a "Foreign.JavaScript" server.
serve
    :: Config               -- ^ Configuration options.
    -> (Window -> IO ())    -- ^ Initialization whenever a client connects.
    -> IO ()
serve config init = httpComm config $ eventLoop $ \w -> do
    init w
    flushCallBuffer w   -- make sure that all `runEval` commands are executed

{-----------------------------------------------------------------------------
    JavaScript
------------------------------------------------------------------------------}
-- | Run a JavaScript function, but do not wait for a result.
runFunction :: Window -> JSFunction () -> IO ()
runFunction w f = bufferRunEval w =<< toCode f


-- | Run a JavaScript function that creates a new object.
-- Return a corresponding 'JSObject' without waiting for the browser
-- to send a result.
--
-- WARNING: This function assumes that the supplied JavaScript code does,
-- in fact, create an object that is new.
unsafeCreateJSObject :: Window -> JSFunction NewJSObject -> IO JSObject
unsafeCreateJSObject w f = do
    g <- wrapImposeStablePtr w f
    bufferRunEval w =<< toCode g
    marshalResult g w JSON.Null

-- | Call a JavaScript function and wait for the result.
callFunction :: Window -> JSFunction a -> IO a
callFunction w f = do
    flushCallBuffer w -- FIXME: Add the code of f to the buffer as well!
    resultJS <- callEval w =<< toCode f
    marshalResult f w resultJS

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
flushCallBuffer :: Window -> IO ()
flushCallBuffer w@Window{..} = do
    code' <- atomically $ do
        code <- readTVar wCallBuffer
        writeTVar wCallBuffer id
        return code
    let code = code' ""
    unless (null code) $
        runEval code

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferRunEval :: Window -> String -> IO ()
bufferRunEval w@Window{..} code = do
    action <- atomically $ do
        mode <- readTVar wCallBufferMode
        case mode of
            BufferRun -> do
                msg <- readTVar wCallBuffer
                writeTVar wCallBuffer (msg . (\s -> ";" ++ code ++ s))
                return Nothing
            NoBuffering -> do
                return $ Just code
    case action of
        Nothing   -> return ()
        Just code -> runEval code
