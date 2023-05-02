module WizardLib.KnowledgeModel.Api.Resource.Event.ChoiceEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.ChoiceEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent

instance ToSchema AddChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch3_q11_cho1

instance ToSchema EditChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch3_q11_cho1

instance ToSchema DeleteChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch3_q11_cho1
