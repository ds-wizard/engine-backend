module Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesJM ()
import Wizard.Api.Resource.User.OnlineUserInfoJM ()
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionDTO

instance FromJSON ClientKnowledgeModelEditorActionDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ClientKnowledgeModelEditorActionDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ServerKnowledgeModelEditorActionDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ServerKnowledgeModelEditorActionDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
