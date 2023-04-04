module Wizard.Api.Handler.Questionnaire.Detail_WS where

import Control.Monad.Except (catchError)
import qualified Data.UUID as U
import Network.WebSockets
import Servant
import Servant.API.WebSocket
import Prelude hiding (log)

import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Handler.Websocket
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionJM ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Util.Websocket

type Detail_WS =
  Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "websocket"
    :> QueryParam "Authorization" String
    :> WebSocket

detail_WS :: Maybe String -> U.UUID -> Maybe String -> Connection -> BaseContextM ()
detail_WS mServerUrl qtnUuid mTokenHeader connection =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      connectionUuid <- initConnection
      catchError
        (putUserOnline qtnUuid connectionUuid connection)
        (sendError connectionUuid connection (U.toString qtnUuid) disconnectUser)
      handleMessage qtnUuid connectionUuid connection

handleMessage :: U.UUID -> U.UUID -> Connection -> AppContextM ()
handleMessage qtnUuid connectionUuid connection =
  handleWebsocketMessage (U.toString qtnUuid) connectionUuid connection handleClose disconnectUser handleAction continue
  where
    continue :: AppContextM ()
    continue = handleMessage qtnUuid connectionUuid connection
    -- ------------------------------------------------------------------------------------
    handleClose :: AppContextM ()
    handleClose = deleteUser qtnUuid connectionUuid
    -- ------------------------------------------------------------------------------------
    handleAction :: ClientQuestionnaireActionDTO -> AppContextM ()
    handleAction (SetContent_ClientQuestionnaireActionDTO reqDto) = do
      log connectionUuid "SetContent"
      setContent qtnUuid connectionUuid reqDto
      handleMessage qtnUuid connectionUuid connection
