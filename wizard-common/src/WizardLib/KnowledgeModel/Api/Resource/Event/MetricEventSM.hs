module WizardLib.KnowledgeModel.Api.Resource.Event.MetricEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.MetricEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Metric.MetricEvent

instance ToSchema AddMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_mtrF

instance ToSchema EditMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_mtrF

instance ToSchema DeleteMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_mtrF
