module Wizard.Api.Handler.KnowledgeModelEditor.Detail_WS where

import Control.Monad.Except (catchError)
import qualified Data.UUID as U
import Network.WebSockets
import Servant
import Servant.API.WebSocket
import Prelude hiding (log)

import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Handler.Websocket
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionDTO
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionJM ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Wizard.Util.Websocket

type Detail_WS =
  Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> "websocket"
    :> QueryParam "Authorization" String
    :> WebSocket

detail_WS :: Maybe String -> U.UUID -> Maybe String -> Connection -> BaseContextM ()
detail_WS mServerUrl editorUuid mTokenHeader connection =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      connectionUuid <- initConnection
      catchError
        (putUserOnline editorUuid connectionUuid connection)
        (sendError connectionUuid connection (U.toString editorUuid) disconnectUser)
      handleMessage editorUuid connectionUuid connection

handleMessage :: U.UUID -> U.UUID -> Connection -> AppContextM ()
handleMessage editorUuid connectionUuid connection =
  handleWebsocketMessage (U.toString editorUuid) connectionUuid connection handleClose disconnectUser handleAction continue
  where
    continue :: AppContextM ()
    continue = handleMessage editorUuid connectionUuid connection
    -- ------------------------------------------------------------------------------------
    handleClose :: AppContextM ()
    handleClose = deleteUser editorUuid connectionUuid
    -- ------------------------------------------------------------------------------------
    handleAction :: ClientKnowledgeModelEditorActionDTO -> AppContextM ()
    handleAction (SetContent_ClientKnowledgeModelEditorActionDTO reqDto) = do
      log connectionUuid "SetContent"
      setContent editorUuid connectionUuid reqDto
      handleMessage editorUuid connectionUuid connection
    handleAction (SetReplies_ClientKnowledgeModelEditorActionDTO reqDto) = do
      log connectionUuid "SetReplies"
      setReplies editorUuid connectionUuid reqDto
      handleMessage editorUuid connectionUuid connection
