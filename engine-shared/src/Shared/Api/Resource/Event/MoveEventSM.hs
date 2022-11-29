module Shared.Api.Resource.Event.MoveEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.MoveEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Move.MoveEvent
import Shared.Util.Swagger

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
