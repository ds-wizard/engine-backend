module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Metric.MetricEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Metric.MetricEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Metric.MetricEvent

instance ToSchema AddMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_mtrF

instance ToSchema EditMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_mtrF

instance ToSchema DeleteMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_mtrF
