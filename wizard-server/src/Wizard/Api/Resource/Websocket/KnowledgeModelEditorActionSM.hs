module Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventSM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesSM ()
import Wizard.Api.Resource.User.OnlineUserInfoSM ()
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionDTO
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionJM ()
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorActions

instance ToSchema ClientKnowledgeModelEditorActionDTO where
  declareNamedSchema = toSwagger ensureOnlineUserAction

instance ToSchema ServerKnowledgeModelEditorActionDTO where
  declareNamedSchema = toSwagger setUserListAction
