{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Foreign.JavaScript.Server (
    httpComm
    ) where

-- import general libraries
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM     as STM
import qualified Control.Exception          as E
import           Control.Monad
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text
import qualified Safe                       as Safe
import           System.Environment
import           System.FilePath
import Data.Maybe (isJust,isNothing)

-- import web libraries
import           Data.Aeson                             ((.=))
import qualified Data.Aeson                    as JSON
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Snap       as WS
import           Snap.Core
import qualified Snap.Http.Server              as Snap
import           Snap.Util.FileServe
import Data.String (fromString)

-- import internal modules
import Foreign.JavaScript.Resources
import Foreign.JavaScript.Types

{-----------------------------------------------------------------------------
    HTTP Server using WebSockets
------------------------------------------------------------------------------}
-- | Run a HTTP server that creates a 'Comm' channel.
httpComm :: Config -> EventLoop -> IO ()
httpComm Config{..} worker = do
    env <- getEnvironment
    let portEnv = Safe.readMay =<< Prelude.lookup "PORT" env
    let dictEnv = Prelude.lookup "DICT_ZLIB" env
    let addrEnv = fmap BS.pack $ Prelude.lookup "ADDR" env
    let porSSLEnv = Safe.readMay =<<  Prelude.lookup "PORT_SSL" env
    let addrSSLEnv = fmap BS.pack $ Prelude.lookup "ADDR_SSL" env
    let keySSLEnv = fmap fromString $ Prelude.lookup "KEY_SSL" env
    let certSSLEnv = fmap fromString $ Prelude.lookup "CERT_SSL" env
    let config = maybe id Snap.setPort (jsPort `mplus` portEnv)
               $ maybe id Snap.setBind (jsAddr `mplus` addrEnv)
               $ maybe id Snap.setSSLBind addrSSLEnv
               $ maybe id Snap.setSSLPort porSSLEnv
               $ maybe id Snap.setSSLKey keySSLEnv
               $ maybe id Snap.setSSLCert certSSLEnv
               $ maybe id (\_ -> Snap.setSSLChainCert False ) certSSLEnv
               $ Snap.setErrorLog  (Snap.ConfigIoLog jsLog)
               $ Snap.setAccessLog (Snap.ConfigIoLog jsLog)
               $ Snap.defaultConfig
    print config
    Snap.httpServe config . route $
        routeResources dictEnv jsCustomHTML jsStatic
        ++ routeWebsockets dictEnv worker

-- | Route the communication between JavaScript and the server
routeWebsockets :: Maybe String -> EventLoop  -> Routes
routeWebsockets dict worker = [("websocket", response)]
    where
    response = do
      requestInfo <- getRequest
      WS.runWebSocketsSnapWith  (WS.defaultConnectionOptions {WS.connectionCompressionOptions = WS.PermessageDeflateCompression  WS.defaultPermessageDeflate } ) $ \ws -> void $ do
        comm <- communicationFromWebSocket dict ws
        worker requestInfo comm
        -- error "Foreign.JavaScript: unreachable code path."

-- | Create 'Comm' channel from WebSocket request.
communicationFromWebSocket :: Maybe String -> WS.PendingConnection -> IO Comm
communicationFromWebSocket dict request = do
    connection <- WS.acceptRequest request
    commIn     <- STM.newTQueueIO   -- outgoing communication
    commOut    <- STM.newTQueueIO   -- incoming communication
    commOpen   <- STM.newTVarIO True

    -- write data to browser
    --
    let
      sendData = forever $ (do
            x <- atomically $ STM.readTQueue commOut
            -- see note [ServerMsg strictness]
            let message =  JSON.encode $ x
            when (not $ LBS.null message ) $
              WS.sendTextData connection  message )

    -- read data from browser
    let readData = forever $  (do
            input <- WS.receiveData connection
            case input of
                "ping" -> WS.sendTextData connection . LBS.pack $  "pong"
                "quit" -> E.throw WS.ConnectionClosed
                input  -> case JSON.decode  input of
                    Just x   -> atomically $ STM.writeTQueue commIn x
                    Nothing  -> error $
                        "Foreign.JavaScript: Couldn't parse JSON input"
                        ++ show input)

        isConnectionClosed  e = isJust (E.fromException e :: Maybe WS.ConnectionException)

    let manageConnection = do
         withAsync sendData $ \_ -> do
            Left e <- waitCatch =<< async readData

            let all :: E.SomeException -> Maybe ()
                all _ = Just ()
            E.tryJust all $ WS.sendClose connection $ LBS.pack "close"
            atomically $ do
              STM.writeTVar   commOpen False
              STM.writeTQueue commIn $
                JSON.object [ "tag" .= ("Quit" :: Text) ] -- write Quit event

        -- there is no point in rethrowing the exception, this thread is dead

    thread <- forkFinally manageConnection
        (\_ -> WS.sendClose connection $ LBS.pack "close")
    -- FIXME: In principle, the thread could be killed *again*
    -- while the `Comm` is being closed, preventing the `commIn` queue
    -- from receiving the "Quit" message
    let commClose = killThread thread

    return $ Comm {..}

{-----------------------------------------------------------------------------
    Resources
------------------------------------------------------------------------------}
type Routes = [(ByteString, Snap ())]

routeResources :: Maybe String -> Maybe FilePath -> Maybe FilePath -> Routes
routeResources dict customHTML staticDir =
  fixHandlers noExpires static ++
        fixHandlers noCache  [("/"            , root)
        ,("/haskell.js"  , writeTextMime (jsDriverCode  dict) "application/javascript")
        ,("/haskell.css" , writeTextMime cssDriverCode "text/css")
        ]
    where
    fixHandlers f routes = [(a,f b) | (a,b) <- routes]
    noCache h = modifyResponse (setHeader "Cache-Control" "max-age=7200") >> h
    noExpires h = modifyResponse (setHeader "Cache-Control" "max-age=7200") >> modifyResponse (setHeader "Expires" "Thu, 15 Apr 2020 20:00:00 GMT") >> h

    static = maybe [] (\dir -> [("/static", serveDirectory dir)]) staticDir

    root = case customHTML of
        Just file -> case staticDir of
            Just dir -> serveFile (dir </> file)
            Nothing  -> logError "Foreign.JavaScript: Cannot use jsCustomHTML file without jsStatic"
        Nothing   -> writeTextMime defaultHtmlFile "text/html"

writeTextMime text mime = do
    modifyResponse (setHeader "Content-type" mime)
    writeText text
