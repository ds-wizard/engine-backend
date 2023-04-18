module Wizard.Api.Resource.Websocket.BranchActionDTO where

import GHC.Generics

import Wizard.Api.Resource.Branch.Event.BranchEventDTO
import Wizard.Model.User.OnlineUserInfo

data ClientBranchActionDTO = SetContent_ClientBranchActionDTO
  { aData :: BranchEventDTO
  }
  deriving (Show, Generic)

data ServerBranchActionDTO
  = SetUserList_ServerBranchActionDTO
      { seData :: [OnlineUserInfo]
      }
  | SetContent_ServerBranchActionDTO
      { scData :: BranchEventDTO
      }
  deriving (Show, Eq, Generic)
