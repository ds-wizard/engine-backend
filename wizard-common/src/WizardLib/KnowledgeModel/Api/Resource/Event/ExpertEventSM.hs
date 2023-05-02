module WizardLib.KnowledgeModel.Api.Resource.Event.ExpertEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.ExpertEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEvent

instance ToSchema AddExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_eAlbert

instance ToSchema EditExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_eAlbert

instance ToSchema DeleteExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_eNikola
