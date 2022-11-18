module Wizard.Service.Questionnaire.Collaboration.CollaborationMapper where

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
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
  :: [WebsocketRecord] -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toSetUserListMessage records record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetUserList_ServerQuestionnaireActionDTO $
      getCollaborators record.connectionUuid record.entityId records

toSetReplyMessage
  :: SetReplyEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toSetReplyMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . SetReplyEventDTO' $
      reqDto

toClearReplyMessage
  :: ClearReplyEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toClearReplyMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . ClearReplyEventDTO' $
      reqDto

toSetPhaseMessage
  :: SetPhaseEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toSetPhaseMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . SetPhaseEventDTO' $
      reqDto

toSetLabelMessage
  :: SetLabelsEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toSetLabelMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . SetLabelsEventDTO' $
      reqDto

toResolveCommentThreadMessage
  :: ResolveCommentThreadEventDTO
  -> WebsocketRecord
  -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toResolveCommentThreadMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . ResolveCommentThreadEventDTO' $
      reqDto

toReopenCommentThreadMessage
  :: ReopenCommentThreadEventDTO
  -> WebsocketRecord
  -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toReopenCommentThreadMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . ReopenCommentThreadEventDTO' $
      reqDto

toDeleteCommentThreadMessage
  :: DeleteCommentThreadEventDTO
  -> WebsocketRecord
  -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toDeleteCommentThreadMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . DeleteCommentThreadEventDTO' $
      reqDto

toAddCommentMessage
  :: AddCommentEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toAddCommentMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . AddCommentEventDTO' $
      reqDto

toEditCommentMessage
  :: EditCommentEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toEditCommentMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . EditCommentEventDTO' $
      reqDto

toDeleteCommentMessage
  :: DeleteCommentEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toDeleteCommentMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerQuestionnaireActionDTO . DeleteCommentEventDTO' $
      reqDto
