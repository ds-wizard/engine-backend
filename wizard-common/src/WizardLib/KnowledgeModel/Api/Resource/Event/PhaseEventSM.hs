module WizardLib.KnowledgeModel.Api.Resource.Event.PhaseEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.PhaseEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Phase.PhaseEvent

instance ToSchema AddPhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_mtrF

instance ToSchema EditPhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_mtrF

instance ToSchema DeletePhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_mtrF
