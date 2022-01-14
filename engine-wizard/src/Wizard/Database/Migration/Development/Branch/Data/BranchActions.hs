module Wizard.Database.Migration.Development.Branch.Data.BranchActions where

import Wizard.Api.Resource.Websocket.BranchActionDTO
import Wizard.Database.Migration.Development.Branch.Data.BranchEvents
import Wizard.Database.Migration.Development.User.Data.Users

ensureOnlineUserAction :: ClientBranchActionDTO
ensureOnlineUserAction = SetContent_ClientBranchActionDTO branchEvent1'

setUserListAction :: ServerBranchActionDTO
setUserListAction = SetUserList_ServerBranchActionDTO [userAlbertOnlineInfo]
