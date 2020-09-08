module Wizard.Service.Questionnaire.Collaboration.CollaborationMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Util.Websocket

toWebsocketMessage :: WebsocketRecord -> content -> WebsocketMessage content
toWebsocketMessage record content =
  WebsocketMessage
    { _websocketMessageConnectionUuid = record ^. connectionUuid
    , _websocketMessageConnection = record ^. connection
    , _websocketMessageEntityId = record ^. entityId
    , _websocketMessageContent = content
    }

toSetUserListMessage ::
     [WebsocketRecord] -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toSetUserListMessage records record =
  toWebsocketMessage record $
  Success_ServerActionDTO . SetUserList_ServerQuestionnaireActionDTO $
  getCollaborators (record ^. connectionUuid) (record ^. entityId) records

toSetReplyMessage ::
     SetReplyEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toSetReplyMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetReply_ServerQuestionnaireActionDTO $ reqDto

toClearReplyMessage ::
     ClearReplyEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toClearReplyMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . ClearReply_ServerQuestionnaireActionDTO $ reqDto

toSetLevelMessage ::
     SetLevelEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toSetLevelMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetLevel_ServerQuestionnaireActionDTO $ reqDto

toSetLabelMessage ::
     SetLabelsEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerQuestionnaireActionDTO)
toSetLabelMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetLabels_ServerQuestionnaireActionDTO $ reqDto
