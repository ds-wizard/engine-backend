module Wizard.Api.Handler.Project.Detail_WS where

import Control.Monad.Except (catchError)
import qualified Data.UUID as U
import Network.WebSockets
import Servant
import Servant.API.WebSocket
import Prelude hiding (log)

import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Handler.Websocket
import Wizard.Api.Resource.Websocket.ProjectMessageDTO
import Wizard.Api.Resource.Websocket.ProjectMessageJM ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Collaboration.ProjectCollaborationService
import Wizard.Util.Websocket

type Detail_WS =
  Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "websocket"
    :> QueryParam "Authorization" String
    :> WebSocket

detail_WS :: Maybe String -> U.UUID -> Maybe String -> Connection -> BaseContextM ()
detail_WS mServerUrl projectUuid mTokenHeader connection =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      connectionUuid <- initConnection
      catchError
        (putUserOnline projectUuid connectionUuid connection)
        (sendError connectionUuid connection (U.toString projectUuid) disconnectUser)
      handleMessage projectUuid connectionUuid connection

handleMessage :: U.UUID -> U.UUID -> Connection -> AppContextM ()
handleMessage projectUuid connectionUuid connection =
  handleWebsocketMessage (U.toString projectUuid) connectionUuid connection handleClose disconnectUser handleAction continue
  where
    continue :: AppContextM ()
    continue = handleMessage projectUuid connectionUuid connection
    -- ------------------------------------------------------------------------------------
    handleClose :: AppContextM ()
    handleClose = deleteUser projectUuid connectionUuid
    -- ------------------------------------------------------------------------------------
    handleAction :: ClientProjectMessageDTO -> AppContextM ()
    handleAction (SetContent_ClientProjectMessageDTO reqDto) = do
      log connectionUuid "SetContent"
      setContent projectUuid connectionUuid reqDto
      handleMessage projectUuid connectionUuid connection
