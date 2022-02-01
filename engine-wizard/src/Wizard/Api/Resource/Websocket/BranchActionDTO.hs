module Wizard.Api.Resource.Websocket.BranchActionDTO where

import GHC.Generics

import Wizard.Api.Resource.Branch.Event.BranchEventDTO
import Wizard.Model.User.OnlineUserInfo

data ClientBranchActionDTO =
  SetContent_ClientBranchActionDTO
    { _setContent_ClientBranchActionDTOData :: BranchEventDTO
    }
  deriving (Show, Generic)

data ServerBranchActionDTO
  = SetUserList_ServerBranchActionDTO
      { _setUserList_ServerBranchActionDTOData :: [OnlineUserInfo]
      }
  | SetContent_ServerBranchActionDTO
      { _setContent_ServerBranchActionDTOData :: BranchEventDTO
      }
  deriving (Show, Eq, Generic)
