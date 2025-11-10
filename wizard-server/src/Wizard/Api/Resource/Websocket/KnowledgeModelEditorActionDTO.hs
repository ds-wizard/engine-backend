module Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionDTO where

import GHC.Generics

import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Wizard.Model.User.OnlineUserInfo

data ClientKnowledgeModelEditorActionDTO
  = SetContent_ClientKnowledgeModelEditorActionDTO
      { scData :: KnowledgeModelEditorWebSocketEventDTO
      }
  | SetReplies_ClientKnowledgeModelEditorActionDTO
      { srData :: SetRepliesDTO
      }
  deriving (Show, Generic)

data ServerKnowledgeModelEditorActionDTO
  = SetUserList_ServerKnowledgeModelEditorActionDTO
      { seData :: [OnlineUserInfo]
      }
  | SetContent_ServerKnowledgeModelEditorActionDTO
      { scData :: KnowledgeModelEditorWebSocketEventDTO
      }
  | SetReplies_ServerKnowledgeModelEditorActionDTO
      { srData :: SetRepliesDTO
      }
  deriving (Show, Eq, Generic)
