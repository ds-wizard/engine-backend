module Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationMapper where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Util.Websocket

toWebsocketMessage :: WebsocketRecord -> content -> WebsocketMessage content
toWebsocketMessage record content =
  WebsocketMessage
    { connectionUuid = record.connectionUuid
    , connection = record.connection
    , entityId = record.entityId
    , content = content
    }

toSetUserListMessage
  :: [WebsocketRecord] -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
toSetUserListMessage records record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetUserList_ServerKnowledgeModelEditorMessageDTO $
      getCollaborators record.connectionUuid record.entityId records

toAddKnowledgeModelEditorWebsocketMessage :: AddKnowledgeModelEditorWebSocketEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
toAddKnowledgeModelEditorWebsocketMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetContent_ServerKnowledgeModelEditorMessageDTO . AddKnowledgeModelEditorWebSocketEventDTO' $ reqDto

toSetRepliesMessage :: SetRepliesDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
toSetRepliesMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetReplies_ServerKnowledgeModelEditorMessageDTO $ reqDto

fromAddKnowledgeModelEditorWebSocketEventDTO :: KnowledgeModelEvent -> U.UUID -> U.UUID -> KnowledgeModelEditorEvent
fromAddKnowledgeModelEditorWebSocketEventDTO KnowledgeModelEvent {..} knowledgeModelEditorUuid tenantUuid = KnowledgeModelEditorEvent {..}

fromSetRepliesEventDTO :: U.UUID -> U.UUID -> M.Map String Reply -> [KnowledgeModelEditorReply]
fromSetRepliesEventDTO knowledgeModelEditorUuid tenantUuid replies = fmap (fromSetReplyEventDTO knowledgeModelEditorUuid tenantUuid) (M.toList replies)

fromSetReplyEventDTO :: U.UUID -> U.UUID -> (String, Reply) -> KnowledgeModelEditorReply
fromSetReplyEventDTO knowledgeModelEditorUuid tenantUuid (path, Reply {..}) = KnowledgeModelEditorReply {..}
