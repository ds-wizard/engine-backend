module Wizard.Api.Handler.Websocket where

import qualified Control.Exception.Base as E
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.UUID as U
import Network.WebSockets
import Prelude hiding (log)

import Shared.Api.Resource.Error.ErrorDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Internal
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Util.Logger
import Wizard.Util.Websocket

initConnection :: AppContextM U.UUID
initConnection = do
  connectionUuid <- liftIO generateUuid
  log connectionUuid "New websocket client"
  return connectionUuid

handleWebsocketMessage ::
     FromJSON reqDto
  => String
  -> U.UUID
  -> Connection
  -> AppContextM ()
  -> (WebsocketMessage Error_ServerActionDTO -> AppContextM ())
  -> (reqDto -> AppContextM ())
  -> AppContextM ()
  -> AppContextM ()
handleWebsocketMessage entityId connectionUuid connection handleClose disconnectUser handleAction continue = do
  log connectionUuid "Waiting for messages..."
  eRawData <- liftIO $ E.try (receiveData connection)
  case eRawData of
    Right rawData -> do
      log connectionUuid "New message recieved"
      case eitherDecode rawData of
        Right action ->
          catchError (handleAction action) (handleError continue connectionUuid connection entityId disconnectUser)
        Left error -> do
          if T.isInfixOf "ping" (T.pack error)
            then return ()
            else do
              log connectionUuid _ERROR_API_WEBSOCKET__DESERIALIZATION_FAILED
              log connectionUuid (show error)
              let error = UserErrorDTO _ERROR_API_WEBSOCKET__DESERIALIZATION_FAILED
              let msg = createErrorWebsocketMessage connectionUuid connection entityId error
              sendMessage disconnectUser msg
          continue
    Left (CloseRequest code bs) -> do
      log connectionUuid (f' "Request to close websocket (code: %s, msg: '%s')" [show code, BSL.unpack bs])
      handleClose
      continue
    Left ConnectionClosed -> do
      log connectionUuid "Connection closed"
      return ()
    Left (ParseException message) -> do
      log connectionUuid "Parse Exception"
      return ()
    Left (UnicodeException message) -> do
      log connectionUuid "Unicode Exception"
      return ()

handleError ::
     AppContextM ()
  -> U.UUID
  -> Connection
  -> String
  -> (WebsocketMessage Error_ServerActionDTO -> AppContextM ())
  -> AppError
  -> AppContextM ()
handleError continue connectionUuid connection entityId disconnectUser error = do
  sendError connectionUuid connection entityId disconnectUser error
  continue

log :: U.UUID -> String -> AppContextM ()
log connectionUuid message = logInfoU _CMP_API (f' "[C:%s] %s" [U.toString connectionUuid, message])
