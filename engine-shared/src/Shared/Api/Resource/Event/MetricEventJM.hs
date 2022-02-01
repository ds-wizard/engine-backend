module Shared.Api.Resource.Event.MetricEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Metric.MetricEvent
import Shared.Util.JSON

instance FromJSON AddMetricEvent where
  parseJSON = simpleParseJSON "_addMetricEvent"

instance ToJSON AddMetricEvent where
  toJSON = simpleToJSON' "_addMetricEvent" "eventType"

-- --------------------------------------------
instance FromJSON EditMetricEvent where
  parseJSON = simpleParseJSON "_editMetricEvent"

instance ToJSON EditMetricEvent where
  toJSON = simpleToJSON' "_editMetricEvent" "eventType"

-- --------------------------------------------
instance FromJSON DeleteMetricEvent where
  parseJSON = simpleParseJSON "_deleteMetricEvent"

instance ToJSON DeleteMetricEvent where
  toJSON = simpleToJSON' "_deleteMetricEvent" "eventType"
