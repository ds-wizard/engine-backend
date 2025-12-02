module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Choice.ChoiceEventJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent

instance FromJSON AddChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
