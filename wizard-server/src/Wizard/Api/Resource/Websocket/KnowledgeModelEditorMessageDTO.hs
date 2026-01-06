module Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO where

import GHC.Generics

import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Wizard.Model.User.OnlineUserInfo

data ClientKnowledgeModelEditorMessageDTO
  = SetContent_ClientKnowledgeModelEditorMessageDTO
      { scData :: KnowledgeModelEditorWebSocketEventDTO
      }
  | SetReplies_ClientKnowledgeModelEditorMessageDTO
      { srData :: SetRepliesDTO
      }
  deriving (Show, Generic)

data ServerKnowledgeModelEditorMessageDTO
  = SetUserList_ServerKnowledgeModelEditorMessageDTO
      { seData :: [OnlineUserInfo]
      }
  | SetContent_ServerKnowledgeModelEditorMessageDTO
      { scData :: KnowledgeModelEditorWebSocketEventDTO
      }
  | SetReplies_ServerKnowledgeModelEditorMessageDTO
      { srData :: SetRepliesDTO
      }
  deriving (Show, Eq, Generic)
