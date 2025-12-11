module Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesJM ()
import Wizard.Api.Resource.User.OnlineUserInfoJM ()
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO

instance FromJSON ClientKnowledgeModelEditorMessageDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ClientKnowledgeModelEditorMessageDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ServerKnowledgeModelEditorMessageDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ServerKnowledgeModelEditorMessageDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
