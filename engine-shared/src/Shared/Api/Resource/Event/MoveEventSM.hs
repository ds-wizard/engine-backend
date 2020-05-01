module Shared.Api.Resource.Event.MoveEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.MoveEventDTO
import Shared.Api.Resource.Event.MoveEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema MoveQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_moveQuestionEventDTO" "eventType" (toDTO m_km1_ch1_q1__to_ch2)

instance ToSchema MoveAnswerEventDTO where
  declareNamedSchema = simpleToSchema'' "_moveAnswerEventDTO" "eventType" (toDTO m_km1_ch1_q2_aYes__to_ch2_q3)

instance ToSchema MoveExpertEventDTO where
  declareNamedSchema = simpleToSchema'' "_moveExpertEventDTO" "eventType" (toDTO m_km1_ch1_q2_eAlbert__to_ch2_q3)

instance ToSchema MoveReferenceEventDTO where
  declareNamedSchema = simpleToSchema'' "_moveReferenceEventDTO" "eventType" (toDTO m_km1_ch1_q2_r1__to_ch2_q3)
