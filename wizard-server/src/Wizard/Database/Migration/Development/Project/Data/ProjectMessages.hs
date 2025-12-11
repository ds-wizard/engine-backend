module Wizard.Database.Migration.Development.Project.Data.ProjectMessages where

import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Api.Resource.Websocket.ProjectMessageDTO
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.Project.Event.ProjectEventMapper

ensureOnlineUserAction :: ClientProjectMessageDTO
ensureOnlineUserAction =
  SetContent_ClientProjectMessageDTO . SetReplyEventChangeDTO' $
    toSetReplyEventChangeDTO (sre_rQ1 project1Uuid)

setUserListAction :: ServerProjectMessageDTO
setUserListAction = SetUserList_ServerProjectMessageDTO [userAlbertOnlineInfo]
