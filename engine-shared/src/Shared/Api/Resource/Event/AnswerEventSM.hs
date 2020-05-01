module Shared.Api.Resource.Event.AnswerEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.AnswerEventDTO
import Shared.Api.Resource.Event.AnswerEventJM ()
import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema AddAnswerEventDTO where
  declareNamedSchema = simpleToSchema'' "_addAnswerEventDTO" "eventType" (toDTO a_km1_ch1_q2_aNo1)

instance ToSchema EditAnswerEventDTO where
  declareNamedSchema = simpleToSchema'' "_editAnswerEventDTO" "eventType" (toDTO e_km1_ch1_q2_aYes1)

instance ToSchema DeleteAnswerEventDTO where
  declareNamedSchema = simpleToSchema'' "_deleteAnswerEventDTO" "eventType" (toDTO d_km1_ch1_q2_aYes1)
