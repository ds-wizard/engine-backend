module Shared.Api.Resource.Event.MetricEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.MetricEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Metric.MetricEvent
import Shared.Util.Swagger

instance ToSchema AddMetricEvent where
  declareNamedSchema = simpleToSchema'' "_addMetricEvent" "eventType" a_km1_mtrF

instance ToSchema EditMetricEvent where
  declareNamedSchema = simpleToSchema'' "_editMetricEvent" "eventType" e_km1_mtrF

instance ToSchema DeleteMetricEvent where
  declareNamedSchema = simpleToSchema'' "_deleteMetricEvent" "eventType" d_km1_mtrF
