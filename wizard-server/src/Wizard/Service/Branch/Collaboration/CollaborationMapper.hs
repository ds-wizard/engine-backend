module Wizard.Service.Branch.Collaboration.CollaborationMapper where

import Wizard.Api.Resource.Branch.Event.BranchEventDTO
import Wizard.Api.Resource.Branch.Event.SetRepliesDTO
import Wizard.Api.Resource.Websocket.BranchActionDTO
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
  :: [WebsocketRecord] -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerBranchActionDTO)
toSetUserListMessage records record =
  toWebsocketMessage record $
    Success_ServerActionDTO . SetUserList_ServerBranchActionDTO $
      getCollaborators record.connectionUuid record.entityId records

toAddBranchMessage
  :: AddBranchEventDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerBranchActionDTO)
toAddBranchMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetContent_ServerBranchActionDTO . AddBranchEventDTO' $ reqDto

toSetRepliesMessage :: SetRepliesDTO -> WebsocketRecord -> WebsocketMessage (Success_ServerActionDTO ServerBranchActionDTO)
toSetRepliesMessage reqDto record =
  toWebsocketMessage record $ Success_ServerActionDTO . SetReplies_ServerBranchActionDTO $ reqDto
