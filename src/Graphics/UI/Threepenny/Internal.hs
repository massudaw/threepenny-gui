{-# LANGUAGE ScopedTypeVariables,ExistentialQuantification,DeriveDataTypeable #-}
module Graphics.UI.Threepenny.Internal (
    -- * Synopsis
    -- | Internal core:
    -- 'UI' monad, integrating FRP and JavaScript FFI. garbage collection

    -- * Documentation
    Window(..), disconnect,
    startGUI,

    UI, runUI, liftIOLater, askWindow,ui,

    FFI, FromJS, ToJS, JSFunction, JSObject, ffi,async,event,
    runFunction, runFunctionDelayed , callFunction,
    CallBufferMode(..), setCallBufferMode, flushCallBuffer,
    ffiExport, debug, timestamp,

    Element(..), fromJSObject, getWindow,
    mkElementNamespace, mkElement, delete, appendChild, clearChildren,forceElement,

    EventData, domEvent,domEventSafe,domEventAsync,domEventH, unsafeFromJSON,
    ) where

import Data.Time
import Data.Unique
import           Control.Applicative                   (Applicative)
import           Control.Monad
import qualified Control.Monad.Trans.State.Strict as State
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import           Data.Dynamic                          (Typeable)

import Foreign.JavaScript.Marshal (JSFunction (..),JSCode (..))
-- import Foreign.JavaScript.Types
import qualified Data.Aeson              as JSON
import qualified Foreign.JavaScript      as JS
import qualified Foreign.RemotePtr       as Foreign

import qualified Reactive.Threepenny     as E
import Reactive.Threepenny    (Dynamic)

import System.IO.Unsafe
import System.Mem (performGC)
import Foreign.JavaScript hiding
    (runFunction,runFunctionDelayed, callFunction, setCallBufferMode, flushCallBuffer
    ,debug, timestamp, Window)

import Debug.Trace
data Wrap f = forall a . Wrap (f a)
{-----------------------------------------------------------------------------
    Custom Window type
------------------------------------------------------------------------------}
-- | The type 'Window' represents a browser window.
data Window = Window
    { wId :: Int
    , wTimeZone :: TimeZone
    , jsWindow    :: JS.Window  -- JavaScript window
    , eDisconnect :: E.Event () -- event that happens when client disconnects
    , wEvents     :: Foreign.Vendor Events
                     -- events associated to 'Element's
    , wAllEvents  :: Foreign.Vendor (Wrap E.Event )
                     -- events associated to 'Element's
    , wChildren   :: Foreign.Vendor ()
                     -- children reachable from 'Element's
    }

-- | Start server for GUI sessions.
startGUI
    :: Config               -- ^ Server configuration.
    -> (Window -> UI ())    -- ^ Action to run whenever a client browser connects.
    ->  (Dynamic Int)
    -> (Window -> IO ())
    -> IO ()
startGUI config init preinit finalizer = JS.serve config ( \w -> do
    -- set up disconnect event
    ((eDisconnect, handleDisconnect),dis) <- E.runDynamic $ E.newEvent

    JS.onDisconnect w $ handleDisconnect ()
    -- make window
    wEvents   <- Foreign.newVendor
    wAllEvents   <- Foreign.newVendor
    wChildren <- Foreign.newVendor
    timezone <- jsTimeZone w
    (windowId ,finini) <- E.runDynamic $ preinit
    let window = Window
            { wId = windowId
            , wTimeZone = timezone
            , jsWindow    = w
            , eDisconnect = eDisconnect
            , wEvents     = wEvents
            , wAllEvents = wAllEvents
            , wChildren   = wChildren
            }

    JS.onDisconnect w $ finalizer window >> sequence_ finini
    -- run initialization
    fin <- E.execDynamic ( runUI window $ init window)
    JS.onDisconnect w $ sequence_ fin
    return (finalizer window))

-- | Event that occurs whenever the client has disconnected,
-- be it by closing the browser window or by exception.
--
-- Note: DOM Elements in a browser window that has been closed
-- can no longer be manipulated.
disconnect :: Window -> E.Event ()
disconnect = eDisconnect



{-----------------------------------------------------------------------------
    Elements
------------------------------------------------------------------------------}
type Events = String -> IO (E.Event JSON.Value)

-- Reachability information for children of an 'Element'.
-- The children of an element are always reachable from this RemotePtr.
type Children = Foreign.RemotePtr ()

data Element = Element
    { toJSObject  :: JS.JSObject -- corresponding JavaScript object
    , elEvents    :: Events      -- FRP event mapping
    , elChildren  :: Children    -- The children of this element
    , elWindow    :: Window      -- Window in which the element was created
    } deriving (Typeable)

instance ToJS Element where
    render = render . toJSObject

getWindow :: Element -> IO Window
getWindow = return . elWindow

-- | Lookup or create reachability information for the children of
-- an element that is represented by a JavaScript object.
getChildren :: JS.JSObject -> Window -> IO Children
getChildren el window@Window{ wChildren = wChildren } =
    Foreign.withRemotePtr el $ \coupon _ -> do
        mptr <- Foreign.lookup coupon wChildren
        case mptr of
            Nothing -> do
                -- Create new pointer for reachability information.
                ptr <- Foreign.newRemotePtr coupon () wChildren
                Foreign.addReachable el ptr
                return ptr
            Just p  ->
                -- Return existing information
                return p

-- | Convert JavaScript object into an Element by attaching relevant information.
-- The JavaScript object may still be subject to garbage collection.
fromJSObject0 :: JS.JSObject -> Window -> Dynamic Element
fromJSObject0 el window = do
    events   <- getEvents   el window
    children <- liftIO$ getChildren el window
    return $ Element el events children  window

-- | Convert JavaScript object into an element.
--
-- FIXME: For the purpose of garbage collection, this element
-- will always be reachable from the root.
fromJSObject :: JS.JSObject -> UI Element
fromJSObject el = do
    window <- askWindow
    ui $ do
        liftIO$ Foreign.addReachable (JS.root $ jsWindow window) el
        fromJSObject0 el window

addEventIO :: String -> JSFunction a -> Bool -> JS.JSObject -> Window -> Dynamic (E.Event a)
addEventIO name fun@(JSFunction _ m ) async el Window{ jsWindow = w, wAllEvents = wAllEvents} = do
    -- Lazily create FRP events whenever they are needed.
    --
    let initializeEvent (name,fun,handler) = do
            handlerPtr <- liftIO $ JS.exportHandler w handler
            -- make handler reachable from element
            liftIO $ Foreign.addReachable el handlerPtr
            E.registerDynamic (Foreign.destroy handlerPtr)
            v <- liftIO $ code fun
            bptr <- liftIO . JS.unsafeCreateJSObject w $
              ffi "Haskell.bind(%1,%2,%3,%4,%5)" el name handlerPtr (unJSCode v)  async
            E.registerDynamic $ JS.runFunction w $
              ffi "Haskell.unbind(%1,%2,%3)" el name bptr


    (e,h) <- E.newEvent
    initializeEvent (name,fun,(h <=< m w ))
    liftIO $ Foreign.withRemotePtr el $ \coupon _ -> do
        ptr <- Foreign.newRemotePtr coupon (Wrap e) wAllEvents
        Foreign.addReachable el ptr

    return e


-- | Add lazy FRP events to a JavaScript object.
addEvents :: JS.JSObject -> Window -> IO Events
addEvents el Window{ jsWindow = w, wEvents = wEvents } = do
    -- Lazily create FRP events whenever they are needed.
    let initializeEvent (name,_,handler) = do
            handlerPtr <- JS.exportHandler w handler
            -- make handler reachable from element
            Foreign.addReachable el handlerPtr
            bptr <- JS.unsafeCreateJSObject w $ traceShow(name) $
                ffi "Haskell.bind(%1,%2,%3)" el name handlerPtr
            addFinalizer handlerPtr $ void $ JS.runFunction w $
              ffi "Haskell.unbind(%1,%2,%3)" el name bptr


    events <- E.newEventsNamed initializeEvent

    -- Create new pointer and add reachability.
    Foreign.withRemotePtr el $ \coupon _ -> do
        ptr <- Foreign.newRemotePtr coupon events wEvents
        Foreign.addReachable el ptr

    return events

-- | Lookup or create lazy events for a JavaScript object.
getEvents :: JS.JSObject -> Window -> E.Dynamic Events
getEvents el window@Window{ wEvents = wEvents } = do
    liftIO$ Foreign.withRemotePtr el $ \coupon _ -> do
        mptr <- Foreign.lookup coupon wEvents
        case mptr of
            Nothing -> addEvents el window
            Just p  -> do
              Foreign.withRemotePtr p $ \_ -> return

-- | Events may carry data. At the moment, they may return
-- a single JSON value, as defined in the "Data.Aeson" module.
type EventData = JSON.Value

-- | Convert event data to a Haskell value.
-- Throws an exception when the data cannot be converted.
unsafeFromJSON :: Show a => JSON.FromJSON a => EventData -> a
unsafeFromJSON x = case  JSON.fromJSON x  of
                JSON.Success y -> y
                e -> error ( show (x,e))

-- | Obtain DOM event for a given element.
domEvent
    :: String
        -- ^ Event name. A full list can be found at
        --   <http://www.w3schools.com/jsref/dom_obj_event.asp>.
        --   Note that the @on@-prefix is not included,
        --   the name is @click@ and so on.
    -> Element          -- ^ Element where the event is to occur.
    -> E.Event EventData
domEvent name el = unsafePerformIO $ elEvents el name
domEventSafe
    :: String
        -- ^ Event name. A full list can be found at
        --   <http://www.w3schools.com/jsref/dom_obj_event.asp>.
        --   Note that the @on@-prefix is not included,
        --   the name is @click@ and so on.
    -> Element          -- ^ Element where the event is to occur.
    -> IO (E.Event EventData)
domEventSafe name el = elEvents el name

domEventH
    :: String
        -- ^ Event name. A full list can be found at
        --   <http://www.w3schools.com/jsref/dom_obj_event.asp>.
        --   Note that the @on@-prefix is not included,
        --   the name is @click@ and so on.
    -> Element          -- ^ Element where the event is to occur.
    -> JSFunction a
    -> UI (E.Event a)
domEventH name el fun = do
  w <- liftIO $getWindow el
  ui $ addEventIO name fun False (toJSObject el) w

domEventAsync
    :: String
        -- ^ Event name. A full list can be found at
        --   <http://www.w3schools.com/jsref/dom_obj_event.asp>.
        --   Note that the @on@-prefix is not included,
        --   the name is @click@ and so on.
    -> Element          -- ^ Element where the event is to occur.
    -> JSAsync a
    -> UI (E.Event a)
domEventAsync name el (JSAsync fun) = do
  w <- liftIO $getWindow el
  ui $ addEventIO name fun True (toJSObject el) w


async :: JSCode
async = JSCode "fun"

event = JSCode "event"



-- | Make a new DOM element with a given tag name.
mkElement :: String -> UI Element
mkElement = mkElementNamespace Nothing

-- | Make a new DOM element with a namespace and a given tag name.
--
-- A namespace 'Nothing' corresponds to the default HTML namespace.
mkElementNamespace :: Maybe String -> String -> UI Element
mkElementNamespace namespace tag = do
    window <- askWindow
    let w = jsWindow window
    ui $ do
        el <- liftIO$ JS.unsafeCreateJSObject w $ case namespace of
            Nothing -> ffi "document.createElement(%1)" tag
            Just ns -> ffi "document.createElementNS(%1,%2)" ns tag
        fromJSObject0 el window

-- | Delete the given element.
delete :: Element -> UI ()
delete el = liftJSWindow  (\w -> do
    JS.runFunction w $ ffi "$(%1).detach()" el
    Foreign.destroy $ toJSObject el)

-- | Remove all child elements.
clearChildren :: Element -> UI ()
clearChildren element = liftJSWindow $ \w -> do
    let el = toJSObject element
    Foreign.withRemotePtr el $ \_ _ -> do
        -- Previous children are no longer reachable from this element
        JS.runFunction w $ ffi "$(%1).contents().detach()" el
        Foreign.clearReachable (elChildren element)

forceElement :: Element -> UI ()
forceElement el = liftJSWindow $ \w -> do
  JS.forceObject w (toJSObject el)

-- | Append a child element.
appendChild :: Element -> Element -> UI ()
appendChild parent child = liftJSWindow $ \w -> do
    -- FIXME: We have to stop the child being reachable from its
    -- /previous/ parent.
    Foreign.addReachable (elChildren parent) (toJSObject child)
    JS.runFunction w $ ffi "$(%1).append($(%2))" (toJSObject parent) (toJSObject child)
    JS.flushChildren w (toJSObject parent) (toJSObject child)


{-----------------------------------------------------------------------------
    UI monad
------------------------------------------------------------------------------}
{- |

User interface elements are created and manipulated in the 'UI' monad.

This monad is essentially just a thin wrapper around the familiar 'IO' monad.
Use the 'liftIO' function to access 'IO' operations like reading
and writing from files.

There are several subtle reasons why Threepenny
uses a custom 'UI' monad instead of the standard 'IO' monad:

* More convenience when calling JavaScript.
The monad keeps track of a browser 'Window' context
in which JavaScript function calls are executed.

* Recursion for functional reactive programming.

-}
newtype UI a = UI { unUI :: Monad.RWST Window [Dynamic ()] () Dynamic a }
    deriving (Typeable)


liftJSWindow :: (JS.Window -> IO a) -> UI a
liftJSWindow f = askWindow >>= liftIO . f . jsWindow

liftJSWindowDyn :: (JS.Window -> E.Dynamic a) -> UI a
liftJSWindowDyn f = askWindow >>= ui . f . jsWindow

instance Functor UI where
    fmap f = UI . fmap f . unUI

instance Applicative UI where
    pure  = return
    (<*>) = ap

instance Monad UI where
    return  = UI . return
    m >>= k = UI $ unUI m >>= unUI . k

instance MonadIO UI where
    liftIO = UI . liftIO

instance MonadFix UI where
    mfix f = UI $ mfix (unUI . f)

instance MonadThrow UI where
    throwM = UI . throwM

instance MonadCatch UI where
    catch m f = UI $ catch (unUI m) (unUI . f)

-- | Execute an 'UI' action in a particular browser window.
-- Also runs all scheduled 'IO' actions.
runUI :: Window -> UI a -> Dynamic a
runUI window m = do
  (a,_,actions) <- Monad.runRWST (unUI m) window ()
  sequence_ actions
  return a

ui f = UI $ lift f

-- | Retrieve current 'Window' context in the 'UI' monad.
askWindow :: UI Window
askWindow = UI Monad.ask

-- | Schedule an 'IO' action to be run later.
liftIOLater :: Dynamic () -> UI ()
liftIOLater x = UI $ Monad.tell [x]


{-----------------------------------------------------------------------------
    FFI
------------------------------------------------------------------------------}
-- | Run the given JavaScript function and carry on. Doesn't block.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
--
-- NOTE: The JavaScript code is subject to buffering,
-- and may not be run immediately. See 'setCallBufferMode'.
runFunction :: JSFunction () -> UI ()
runFunction fun = liftJSWindow $ \w -> JS.runFunction w fun

runFunctionDelayed :: Element -> JSFunction () -> UI ()
runFunctionDelayed el fun = liftJSWindow $ \w -> JS.runFunctionDelayed w (toJSObject el) fun
-- | Run the given JavaScript function and wait for results. Blocks.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
callFunction :: JSFunction a -> UI a
callFunction fun = liftJSWindow $ \w -> JS.callFunction w fun

-- | Set the call buffering mode for the browser window.
setCallBufferMode :: CallBufferMode -> UI ()
setCallBufferMode x = liftJSWindow $ \w -> JS.setCallBufferMode w x

-- | Flush the call buffer,
-- i.e. send all outstanding JavaScript to the client in one single message.
flushCallBuffer :: UI ()
flushCallBuffer = liftJSWindow $ \w -> JS.flushCallBuffer w

-- | Export the given Haskell function so that it can be called
-- from JavaScript code.
--
-- FIXME: At the moment, the function is not garbage collected.
ffiExport :: JS.IsHandler a => a -> UI JSObject
ffiExport fun = liftJSWindowDyn $ \w -> do
    handlerPtr <- liftIO$ JS.exportHandler w fun
    liftIO$ Foreign.addReachable (JS.root w) handlerPtr
    E.registerDynamic (Foreign.destroy handlerPtr)
    return handlerPtr

-- | Print a message on the client console if the client has debugging enabled.
debug :: String -> UI ()
debug s = liftJSWindow $ \w -> JS.debug w s

-- | Print a timestamp and the difference to the previous timestamp
-- on the client console if the client has debugging enabled.
timestamp :: UI ()
timestamp = liftJSWindow JS.timestamp

jsTimeZone :: JS.Window -> IO TimeZone
jsTimeZone  w = return utc {-do
  fmap ((\ i -> TimeZone (negate i) False "") .from )$ JS.callFunction w $ ffi "new Date().getTimezoneOffset()"
  where
    from s = let JSON.Success x =JSON.fromJSON s in x
-}
uiTimeZome = wTimeZone <$> askWindow

