module Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO

instance ToJSON KnowledgeModelEditorWebSocketEventDTO where
  toJSON = toSumJSON

instance FromJSON KnowledgeModelEditorWebSocketEventDTO where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "AddKnowledgeModelEditorWebSocketEvent" -> parseJSON (Object o) >>= \event -> return (AddKnowledgeModelEditorWebSocketEventDTO' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON AddKnowledgeModelEditorWebSocketEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddKnowledgeModelEditorWebSocketEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
