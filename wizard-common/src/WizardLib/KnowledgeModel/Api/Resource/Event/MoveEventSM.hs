module WizardLib.KnowledgeModel.Api.Resource.Event.MoveEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.MoveEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent

instance ToSchema MoveQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch1_q1__to_ch2

instance ToSchema MoveAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch1_q2_aYes__to_ch2_q3

instance ToSchema MoveChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch3_q11_cho1__to_ch3_q12

instance ToSchema MoveExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch1_q2_eAlbert__to_ch2_q3

instance ToSchema MoveReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch1_q2_r1__to_ch2_q3
