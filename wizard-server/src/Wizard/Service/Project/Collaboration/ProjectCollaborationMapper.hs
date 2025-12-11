module Wizard.Service.Project.Collaboration.ProjectCollaborationMapper where

import Wizard.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Wizard.Api.Resource.Project.Event.ProjectEventDTO
import Wizard.Api.Resource.Websocket.ProjectMessageDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Model.Project.File.ProjectFileSimple
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
  :: [WebsocketRecord] -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toSetUserListMessage records record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetUserList_ServerProjectMessageDTO $
      getCollaborators record.connectionUuid record.entityId records

toSetReplyMessage
  :: SetReplyEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toSetReplyMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . SetReplyEventDTO' $
      reqDto

toClearReplyMessage
  :: ClearReplyEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toClearReplyMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . ClearReplyEventDTO' $
      reqDto

toSetPhaseMessage
  :: SetPhaseEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toSetPhaseMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . SetPhaseEventDTO' $
      reqDto

toSetLabelMessage
  :: SetLabelsEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toSetLabelMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . SetLabelsEventDTO' $
      reqDto

toResolveCommentThreadMessage
  :: ResolveCommentThreadEventDTO
  -> WebsocketRecord
  -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toResolveCommentThreadMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . ResolveCommentThreadEventDTO' $
      reqDto

toReopenCommentThreadMessage
  :: ReopenCommentThreadEventDTO
  -> WebsocketRecord
  -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toReopenCommentThreadMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . ReopenCommentThreadEventDTO' $
      reqDto

toAssignCommentThreadMessage
  :: AssignCommentThreadEventDTO
  -> WebsocketRecord
  -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toAssignCommentThreadMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . AssignCommentThreadEventDTO' $
      reqDto

toDeleteCommentThreadMessage
  :: DeleteCommentThreadEventDTO
  -> WebsocketRecord
  -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toDeleteCommentThreadMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . DeleteCommentThreadEventDTO' $
      reqDto

toAddCommentMessage
  :: AddCommentEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toAddCommentMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . AddCommentEventDTO' $
      reqDto

toEditCommentMessage
  :: EditCommentEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toEditCommentMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . EditCommentEventDTO' $
      reqDto

toDeleteCommentMessage
  :: DeleteCommentEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toDeleteCommentMessage reqDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . DeleteCommentEventDTO' $
      reqDto

toSetProjectMessage
  :: ProjectDetailWsDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toSetProjectMessage resWsDto record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetProject_ServerProjectMessageDTO $
      resWsDto

toAddFileMessage :: ProjectFileSimple -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerProjectMessageDTO)
toAddFileMessage file record =
  toWebsocketMessage record $
    Success_ServerActionDTO . AddFile_ServerProjectMessageDTO $
      file
