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
  declareNamedSchema = simpleToSchema'' "_moveQuestionEvent" "eventType" m_km1_ch1_q1__to_ch2

instance ToSchema MoveAnswerEvent where
  declareNamedSchema = simpleToSchema'' "_moveAnswerEvent" "eventType" m_km1_ch1_q2_aYes__to_ch2_q3

instance ToSchema MoveExpertEvent where
  declareNamedSchema = simpleToSchema'' "_moveExpertEvent" "eventType" m_km1_ch1_q2_eAlbert__to_ch2_q3

instance ToSchema MoveReferenceEvent where
  declareNamedSchema = simpleToSchema'' "_moveReferenceEvent" "eventType" m_km1_ch1_q2_r1__to_ch2_q3
