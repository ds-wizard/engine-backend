module Wizard.Util.Websocket where

import qualified Control.Exception.Base as E
import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON, encode)
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.WebSockets (Connection, sendTextData)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord

-- --------------------------------
-- PRIVATE
-- --------------------------------
-- Websocket
broadcast
  :: ToJSON a
  => String
  -> [WebsocketRecord]
  -> (WebsocketRecord -> WebsocketMessage a)
  -> (WebsocketMessage a -> AppContextM ())
  -> AppContextM ()
broadcast entityUuid records toMessage disconnectUser =
  traverse_ (sendMessage disconnectUser . toMessage) (filter (filterByEntityId entityUuid) records)

sendMessage :: ToJSON a => (WebsocketMessage a -> AppContextM ()) -> WebsocketMessage a -> AppContextM ()
sendMessage disconnectUser msg = do
  logWS msg.connectionUuid "Sending message..."
  eResult <- liftIO $ E.try $ sendTextData msg.connection (encode msg.content)
  case eResult of
    Right _ -> do
      logWS msg.connectionUuid "Successfully sent"
      return ()
    Left (e :: E.SomeException) -> do
      logWS msg.connectionUuid "Failed to sent. Start disconnecting"
      disconnectUser msg
      logWS msg.connectionUuid "Successfully disconnected"
      return ()

sendError
  :: U.UUID
  -> Connection
  -> String
  -> (WebsocketMessage Error_ServerActionDTO -> AppContextM ())
  -> AppError
  -> AppContextM ()
sendError connectionUuid connection entityId disconnectUser error@ForbiddenError {} = do
  let msg = createErrorWebsocketMessage connectionUuid connection entityId error
  sendMessage disconnectUser msg
  disconnectUser msg
sendError connectionUuid connection entityId disconnectUser error =
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error

-- Filter
exceptMyself :: U.UUID -> WebsocketRecord -> Bool
exceptMyself myConnectionUuid record = record.connectionUuid /= myConnectionUuid

filterByEntityId :: String -> WebsocketRecord -> Bool
filterByEntityId questionnaireUuid record = questionnaireUuid == record.entityId

-- Accessors
getCollaborators :: U.UUID -> String -> [WebsocketRecord] -> [OnlineUserInfo]
getCollaborators connectionUuid entityId =
  fmap (.user) . filter (exceptMyself connectionUuid) . filter (filterByEntityId entityId)

-- Mapper
createErrorWebsocketMessage :: U.UUID -> Connection -> String -> AppError -> WebsocketMessage Error_ServerActionDTO
createErrorWebsocketMessage connectionUuid connection entityId error =
  WebsocketMessage
    { connectionUuid = connectionUuid
    , connection = connection
    , entityId = entityId
    , content = Error_ServerActionDTO error
    }

-- Logs
logWS connectionUuid message = do
  serverConfig <- asks serverConfig
  when
    serverConfig.logging.websocketDebug
    (logInfoI _CMP_SERVICE (f' "[C:%s] %s" [U.toString connectionUuid, message]))
