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
    Window,requestInfo , root,

    -- * JavaScript FFI
    JavaScriptException(..),JSCode(..),ToJS(..), FromJS, JSFunction(..),emptyFunction, JSObject,toCode,
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
import           Foreign.JavaScript.CallBuffer
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
import GHC.Conc
{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
-- | Run a "Foreign.JavaScript" server.
serve
    :: Config               -- ^ Configuration options.
    -> (Window -> IO ())-- ^ Initialization whenever a client connects.
    -> IO ()
serve config init = httpComm config $ eventLoop $ \w -> do
  init w
  flushCallBuffer w   -- make sure that all `runEval` commands are executed
  return ()

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
runFunctionDelayed w js f = bufferRunEvalMethod w js =<< toCode f



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
    marshalResult g w JSON.Null

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
