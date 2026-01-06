module Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventSM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesSM ()
import Wizard.Api.Resource.User.OnlineUserInfoSM ()
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorMessages

instance ToSchema ClientKnowledgeModelEditorMessageDTO where
  declareNamedSchema = toSwagger ensureOnlineUserAction

instance ToSchema ServerKnowledgeModelEditorMessageDTO where
  declareNamedSchema = toSwagger setUserListAction
