module WizardLib.KnowledgeModel.Api.Resource.Event.AnswerEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.AnswerEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent

instance ToSchema AddAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_aNo1

instance ToSchema EditAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_aYes1

instance ToSchema DeleteAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_aYes1
