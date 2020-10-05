module Wizard.Util.Websocket where

import qualified Control.Exception.Base as E
import Control.Lens ((^.))
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson (ToJSON, encode)
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.WebSockets (Connection, sendTextData)

import LensesConfig
import Shared.Api.Resource.Error.ErrorDTO
import Shared.Localization.Locale
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Util.Logger

-- --------------------------------
-- PRIVATE
-- --------------------------------
-- Websocket
broadcast ::
     ToJSON a
  => String
  -> [WebsocketRecord]
  -> (WebsocketRecord -> WebsocketMessage a)
  -> (WebsocketMessage a -> AppContextM ())
  -> AppContextM ()
broadcast entityUuid records toMessage disconnectUser =
  traverse_ (sendMessage disconnectUser . toMessage) (filter (filterByEntityId entityUuid) records)

sendMessage :: ToJSON a => (WebsocketMessage a -> AppContextM ()) -> WebsocketMessage a -> AppContextM ()
sendMessage disconnectUser msg = do
  logWS (msg ^. connectionUuid) "Sending message..."
  eResult <- liftIO $ E.try $ sendTextData (msg ^. connection) (encode $ msg ^. content)
  case eResult of
    Right _ -> do
      logWS (msg ^. connectionUuid) "Successfully sent"
      return ()
    Left (e :: E.SomeException) -> do
      logWS (msg ^. connectionUuid) "Failed to sent. Start disconnecting"
      disconnectUser msg
      logWS (msg ^. connectionUuid) "Successfully disconnected"
      return ()

sendError ::
     U.UUID
  -> Connection
  -> String
  -> (WebsocketMessage Error_ServerActionDTO -> AppContextM ())
  -> AppError
  -> AppContextM ()
sendError connectionUuid connection entityId disconnectUser AcceptedError =
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId AcceptedErrorDTO
sendError connectionUuid connection entityId disconnectUser (FoundError url) = do
  let error = FoundErrorDTO url
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error
sendError connectionUuid connection entityId disconnectUser (ValidationError formErrorRecords fieldErrorRecords) = do
  context <- ask
  let ls = context ^. localization'
  let formErrors = fmap (locale ls) formErrorRecords
  let localeTuple (k, v) = (k, locale ls v)
  let fieldErrors = fmap localeTuple fieldErrorRecords
  let error = ValidationErrorDTO formErrors fieldErrors
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error
sendError connectionUuid connection entityId disconnectUser (UserError localeRecord) = do
  localizedMessage <- localizeRecord localeRecord
  let error = UserErrorDTO localizedMessage
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error
sendError connectionUuid connection entityId disconnectUser (UnauthorizedError localeRecord) = do
  localizedMessage <- localizeRecord localeRecord
  let error = UnauthorizedErrorDTO localizedMessage
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error
sendError connectionUuid connection entityId disconnectUser (ForbiddenError localeRecord) = do
  localizedMessage <- localizeRecord localeRecord
  let error = ForbiddenErrorDTO localizedMessage
  let msg = createErrorWebsocketMessage connectionUuid connection entityId error
  sendMessage disconnectUser msg
  disconnectUser msg
sendError connectionUuid connection entityId disconnectUser (NotExistsError localeRecord) = do
  localizedMessage <- localizeRecord localeRecord
  let error = NotExistsErrorDTO localizedMessage
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error
sendError connectionUuid connection entityId disconnectUser (GeneralServerError errorMessage) = do
  let error = GeneralServerErrorDTO errorMessage
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error
sendError connectionUuid connection entityId disconnectUser (HttpClientError status message) = do
  let error = HttpClientErrorDTO status message
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error

-- Filter
exceptMyself :: U.UUID -> WebsocketRecord -> Bool
exceptMyself myConnectionUuid record = (record ^. connectionUuid) /= myConnectionUuid

filterByEntityId :: String -> WebsocketRecord -> Bool
filterByEntityId questionnaireUuid record = questionnaireUuid == (record ^. entityId)

-- Accessors
getCollaborators :: U.UUID -> String -> [WebsocketRecord] -> [OnlineUserInfo]
getCollaborators connectionUuid entityId =
  fmap (^. user) . filter (exceptMyself connectionUuid) . filter (filterByEntityId entityId)

-- Mapper
localizeRecord localeRecord = do
  context <- ask
  let ls = context ^. localization'
  return $ locale ls localeRecord

createErrorWebsocketMessage :: U.UUID -> Connection -> String -> ErrorDTO -> WebsocketMessage Error_ServerActionDTO
createErrorWebsocketMessage connectionUuid connection entityId error =
  WebsocketMessage
    { _websocketMessageConnectionUuid = connectionUuid
    , _websocketMessageConnection = connection
    , _websocketMessageEntityId = entityId
    , _websocketMessageContent = Error_ServerActionDTO error
    }

-- Logs
logWS connectionUuid message = logInfoU _CMP_SERVICE (f' "[C:%s] %s" [U.toString connectionUuid, message])
