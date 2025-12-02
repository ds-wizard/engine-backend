module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent

instance FromJSON AddKnowledgeModelEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddKnowledgeModelEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditKnowledgeModelEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditKnowledgeModelEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
