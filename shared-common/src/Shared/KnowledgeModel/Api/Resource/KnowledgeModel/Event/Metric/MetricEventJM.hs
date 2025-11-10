module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Metric.MetricEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Metric.MetricEvent

instance FromJSON AddMetricEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddMetricEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditMetricEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditMetricEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
