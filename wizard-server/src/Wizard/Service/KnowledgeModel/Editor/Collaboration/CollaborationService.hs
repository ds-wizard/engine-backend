module Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationService where

import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageJM ()
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Cache.KnowledgeModelEditorWebsocketCache
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO (insertKnowledgeModelEvent)
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationAcl
import Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationMapper
import Wizard.Service.Websocket.WebsocketService
import Wizard.Util.Websocket

putUserOnline :: U.UUID -> U.UUID -> Connection -> AppContextM ()
putUserOnline kmEditorUuid connectionUuid connection = do
  myself <- createRecord connectionUuid connection (U.toString kmEditorUuid) EditorWebsocketPerm []
  checkViewPermission myself
  _ <- findKnowledgeModelEditorByUuid kmEditorUuid
  addToCache myself
  logWS connectionUuid "New user added to the list"
  setUserList kmEditorUuid connectionUuid

deleteUser :: U.UUID -> U.UUID -> AppContextM ()
deleteUser kmEditorUuid connectionUuid = do
  deleteFromCache connectionUuid
  setUserList kmEditorUuid connectionUuid

setUserList :: U.UUID -> U.UUID -> AppContextM ()
setUserList kmEditorUuid connectionUuid = do
  logWS connectionUuid "Informing other users about user list changes"
  records <- getAllFromCache
  broadcast (U.toString kmEditorUuid) records (toSetUserListMessage records) disconnectUser
  logWS connectionUuid "Informed completed"

logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged :: U.UUID -> AppContextM ()
logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditorUuid = do
  records <- getAllFromCache
  let error = NotExistsError $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__COLLABORATION__FORCE_DISCONNECT (U.toString kmEditorUuid)
  traverse_ (logOut error) records
  where
    logOut :: AppError -> WebsocketRecord -> AppContextM ()
    logOut error record =
      when
        (record.entityId == U.toString kmEditorUuid)
        (sendError record.connectionUuid record.connection record.entityId disconnectUser error)

-- --------------------------------
setContent :: U.UUID -> U.UUID -> KnowledgeModelEditorWebSocketEventDTO -> AppContextM ()
setContent kmEditorUuid connectionUuid reqDto =
  case reqDto of
    AddKnowledgeModelEditorWebSocketEventDTO' event -> addKnowledgeModelEvent kmEditorUuid connectionUuid event

addKnowledgeModelEvent :: U.UUID -> U.UUID -> AddKnowledgeModelEditorWebSocketEventDTO -> AppContextM ()
addKnowledgeModelEvent kmEditorUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  tenantUuid <- asks currentTenantUuid
  let kmEditorEvent = fromAddKnowledgeModelEditorWebSocketEventDTO reqDto.event kmEditorUuid tenantUuid
  insertKnowledgeModelEvent kmEditorEvent
  records <- getAllFromCache
  broadcast (U.toString kmEditorUuid) records (toAddKnowledgeModelEditorWebsocketMessage reqDto) disconnectUser

-- --------------------------------
setReplies :: U.UUID -> U.UUID -> SetRepliesDTO -> AppContextM ()
setReplies kmEditorUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  tenantUuid <- asks currentTenantUuid
  let kmEditorReplies = fromSetRepliesEventDTO kmEditorUuid tenantUuid reqDto.replies
  updateKnowledgeModelRepliesByEditorUuid kmEditorUuid kmEditorReplies
  records <- getAllFromCache
  broadcast (U.toString kmEditorUuid) records (toSetRepliesMessage reqDto) disconnectUser

-- --------------------------------
disconnectUser :: ToJSON resDto => WebsocketMessage resDto -> AppContextM ()
disconnectUser msg = deleteUser (u' msg.entityId) msg.connectionUuid
