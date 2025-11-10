module Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationMapper where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply
import Wizard.Model.Questionnaire.QuestionnaireReply
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
  :: [WebsocketRecord] -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerKnowledgeModelEditorActionDTO)
toSetUserListMessage records record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetUserList_ServerKnowledgeModelEditorActionDTO $
      getCollaborators record.connectionUuid record.entityId records

toAddKnowledgeModelEditorWebsocketMessage :: AddKnowledgeModelEditorWebSocketEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerKnowledgeModelEditorActionDTO)
toAddKnowledgeModelEditorWebsocketMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetContent_ServerKnowledgeModelEditorActionDTO . AddKnowledgeModelEditorWebSocketEventDTO' $ reqDto

toSetRepliesMessage :: SetRepliesDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerKnowledgeModelEditorActionDTO)
toSetRepliesMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetReplies_ServerKnowledgeModelEditorActionDTO $ reqDto

fromAddKnowledgeModelEditorWebSocketEventDTO :: KnowledgeModelEvent -> U.UUID -> U.UUID -> KnowledgeModelEditorEvent
fromAddKnowledgeModelEditorWebSocketEventDTO KnowledgeModelEvent {..} knowledgeModelEditorUuid tenantUuid = KnowledgeModelEditorEvent {..}

fromSetRepliesEventDTO :: U.UUID -> U.UUID -> M.Map String Reply -> [KnowledgeModelEditorReply]
fromSetRepliesEventDTO knowledgeModelEditorUuid tenantUuid replies = fmap (fromSetReplyEventDTO knowledgeModelEditorUuid tenantUuid) (M.toList replies)

fromSetReplyEventDTO :: U.UUID -> U.UUID -> (String, Reply) -> KnowledgeModelEditorReply
fromSetReplyEventDTO knowledgeModelEditorUuid tenantUuid (path, Reply {..}) = KnowledgeModelEditorReply {..}
