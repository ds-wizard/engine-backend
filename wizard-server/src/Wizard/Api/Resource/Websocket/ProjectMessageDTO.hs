module Wizard.Api.Resource.Websocket.ProjectMessageDTO where

import GHC.Generics

import Wizard.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Api.Resource.Project.Event.ProjectEventDTO
import Wizard.Model.Project.File.ProjectFileSimple
import Wizard.Model.User.OnlineUserInfo

data ClientProjectMessageDTO = SetContent_ClientProjectMessageDTO
  { aData :: ProjectEventChangeDTO
  }
  deriving (Show, Generic)

data ServerProjectMessageDTO
  = SetUserList_ServerProjectMessageDTO
      { ouiData :: [OnlineUserInfo]
      }
  | SetContent_ServerProjectMessageDTO
      { qeData :: ProjectEventDTO
      }
  | SetProject_ServerProjectMessageDTO
      { sqData :: ProjectDetailWsDTO
      }
  | AddFile_ServerProjectMessageDTO
      { adData :: ProjectFileSimple
      }
  deriving (Show, Eq, Generic)
