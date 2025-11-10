module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEvent

instance FromJSON AddExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
