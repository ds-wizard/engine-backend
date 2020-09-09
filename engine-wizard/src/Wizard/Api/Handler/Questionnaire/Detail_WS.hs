module Wizard.Api.Handler.Questionnaire.Detail_WS where

import Control.Monad.Except (catchError)
import qualified Data.UUID as U
import Network.WebSockets
import Prelude hiding (log)
import Servant
import Servant.API.WebSocket

import Wizard.Api.Handler.Common
import Wizard.Api.Handler.Websocket
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionJM ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Util.Websocket

type Detail_WS
   = "questionnaires"
     :> Capture "qtnUuid" String
     :> "websocket"
     :> QueryParam "Authorization" String
     :> WebSocket

detail_WS :: String -> Maybe String -> Connection -> BaseContextM ()
detail_WS qtnUuid mTokenHeader connection =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService $ do
      connectionUuid <- initConnection
      catchError
        (putUserOnline qtnUuid connectionUuid connection)
        (sendError connectionUuid connection qtnUuid disconnectUser)
      handleMessage qtnUuid connectionUuid connection

handleMessage :: String -> U.UUID -> Connection -> AppContextM ()
handleMessage qtnUuid connectionUuid connection =
  handleWebsocketMessage qtnUuid connectionUuid connection handleClose disconnectUser handleAction continue
  where
    continue :: AppContextM ()
    continue = handleMessage qtnUuid connectionUuid connection
    -- ------------------------------------------------------------------------------------
    handleClose :: AppContextM ()
    handleClose = deleteUser qtnUuid connectionUuid
    -- ------------------------------------------------------------------------------------
    handleAction :: ClientQuestionnaireActionDTO -> AppContextM ()
    handleAction (SetReply_ClientQuestionnaireActionDTO reqDto) = do
      log connectionUuid "SetReply"
      setReply qtnUuid connectionUuid reqDto
      handleMessage qtnUuid connectionUuid connection
    handleAction (ClearReply_ClientQuestionnaireActionDTO reqDto) = do
      log connectionUuid "ClearReply"
      clearReply qtnUuid connectionUuid reqDto
      handleMessage qtnUuid connectionUuid connection
    handleAction (SetLevel_ClientQuestionnaireActionDTO reqDto) = do
      log connectionUuid "ClearReply"
      setLevel qtnUuid connectionUuid reqDto
      handleMessage qtnUuid connectionUuid connection
    handleAction (SetLabels_ClientQuestionnaireActionDTO reqDto) = do
      log connectionUuid "ClearReply"
      setLabel qtnUuid connectionUuid reqDto
      handleMessage qtnUuid connectionUuid connection
