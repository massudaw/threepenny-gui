import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Control.Monad
import Data.Functor
import Data.List.Extra
import Data.Time
import Data.IORef
import Prelude hiding (catch)

import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)

{-----------------------------------------------------------------------------
    Chat
------------------------------------------------------------------------------}

main :: IO ()
main = do
    static   <- getStaticDir
    messages <- Chan.newChan
    startGUI defaultConfig
        { jsCustomHTML = Just "chat.html"
        , jsStatic     = Just static
        , jsCallBufferMode = BufferAll
        } ( setup messages) (return 1)

type Message = (UTCTime, String, String)

setup :: Chan Message -> Window -> UI ()
setup globalMsgs window = do
    msgs <- liftIO $ Chan.dupChan globalMsgs

    return window # set title "Chat"

    (nickRef, nickname) <- mkNickname
    messageArea         <- mkMessageArea msgs nickRef

    o <- sequence [ UI.div #. "header"   #+ [string "Threepenny Chat"]
        , UI.div #. "gradient"
        , viewSource
        , element nickname
         , element messageArea
        ]
    addBody o

    messageReceiver <- liftIO $ forkIO $ receiveMessages window msgs messageArea

    ui$registerDynamic $ do
        print "Run disconnect"
        killThread messageReceiver
        now   <- getCurrentTime
        nick  <- readIORef nickRef
        Chan.writeChan msgs (now,nick,"( left the conversation )")


receiveMessages w msgs messageArea = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        execDynamic $ runUI w $ do
          element messageArea #+ [mkMessage msg]
          UI.scrollToBottom messageArea

mkMessageArea :: Chan Message -> IORef String -> UI Element
mkMessageArea msgs nickname = do
    input <- UI.textarea #. "send-textarea"
    sv <- UI.sendValue input
    onEvent sv $ (. trim) $ \content -> do
        element input # set value ""
        when (not (null content)) $ liftIO $ do
            now  <- getCurrentTime
            nick <- readIORef nickname
            when (not (null nick)) $
                Chan.writeChan msgs (now,nick,content)

    o <- UI.div #. "send-area" # set children [input]
    UI.div #. "message-area" # set children [o]


mkNickname :: UI (IORef String, Element)
mkNickname = do
    input  <- UI.input #. "name-input"
    el     <- UI.div   #. "name-area"  #+
                [ UI.span  #. "name-label" #+ [string "Your name "]
                , element input
                ]
    UI.setFocus input

    nick <- liftIO $ newIORef ""
    ku <- UI.onChangeE input
    onEvent ku $ \i -> liftIO . writeIORef nick . trim $ i
    return (nick,el)

mkMessage :: Message -> UI Element
mkMessage (timestamp, nick, content) =
    UI.div #. "message" #+
        [ UI.div #. "timestamp" #+ [string $ show timestamp]
        , UI.div #. "name"      #+ [string $ nick ++ " says:"]
        , UI.div #. "content"   #+ [string content]
        ]

viewSource :: UI Element
viewSource =
    UI.anchor #. "view-source" # set UI.href url #+ [string "View source code"]
    where
    url = samplesURL ++ "Chat.hs"
