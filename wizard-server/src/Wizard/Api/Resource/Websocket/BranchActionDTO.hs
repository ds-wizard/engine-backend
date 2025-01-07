module Wizard.Api.Resource.Websocket.BranchActionDTO where

import GHC.Generics

import Wizard.Api.Resource.Branch.Event.BranchEventDTO
import Wizard.Api.Resource.Branch.Event.SetRepliesDTO
import Wizard.Model.User.OnlineUserInfo

data ClientBranchActionDTO
  = SetContent_ClientBranchActionDTO
      { scData :: BranchEventDTO
      }
  | SetReplies_ClientBranchActionDTO
      { srData :: SetRepliesDTO
      }
  deriving (Show, Generic)

data ServerBranchActionDTO
  = SetUserList_ServerBranchActionDTO
      { seData :: [OnlineUserInfo]
      }
  | SetContent_ServerBranchActionDTO
      { scData :: BranchEventDTO
      }
  | SetReplies_ServerBranchActionDTO
      { srData :: SetRepliesDTO
      }
  deriving (Show, Eq, Generic)
