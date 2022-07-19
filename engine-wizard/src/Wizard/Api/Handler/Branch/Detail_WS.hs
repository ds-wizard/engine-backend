module Wizard.Api.Handler.Branch.Detail_WS where

import Control.Monad.Except (catchError)
import qualified Data.UUID as U
import Network.WebSockets
import Prelude hiding (log)
import Servant
import Servant.API.WebSocket

import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Handler.Websocket
import Wizard.Api.Resource.Websocket.BranchActionDTO
import Wizard.Api.Resource.Websocket.BranchActionJM ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.Collaboration.CollaborationService
import Wizard.Util.Websocket

type Detail_WS
   = Header "Host" String
     :> "branches"
     :> Capture "branchUuid" String
     :> "websocket"
     :> QueryParam "Authorization" String
     :> WebSocket

detail_WS :: Maybe String -> String -> Maybe String -> Connection -> BaseContextM ()
detail_WS mServerUrl branchUuid mTokenHeader connection =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      connectionUuid <- initConnection
      catchError
        (putUserOnline branchUuid connectionUuid connection)
        (sendError connectionUuid connection branchUuid disconnectUser)
      handleMessage branchUuid connectionUuid connection

handleMessage :: String -> U.UUID -> Connection -> AppContextM ()
handleMessage branchUuid connectionUuid connection =
  handleWebsocketMessage branchUuid connectionUuid connection handleClose disconnectUser handleAction continue
  where
    continue :: AppContextM ()
    continue = handleMessage branchUuid connectionUuid connection
    -- ------------------------------------------------------------------------------------
    handleClose :: AppContextM ()
    handleClose = deleteUser branchUuid connectionUuid
    -- ------------------------------------------------------------------------------------
    handleAction :: ClientBranchActionDTO -> AppContextM ()
    handleAction (SetContent_ClientBranchActionDTO reqDto) = do
      log connectionUuid "SetContent"
      setContent branchUuid connectionUuid reqDto
      handleMessage branchUuid connectionUuid connection
