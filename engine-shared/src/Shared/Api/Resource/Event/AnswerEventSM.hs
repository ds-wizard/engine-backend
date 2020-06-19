module Shared.Api.Resource.Event.AnswerEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.AnswerEventJM ()
import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Util.Swagger

instance ToSchema AddAnswerEvent where
  declareNamedSchema = simpleToSchema'' "_addAnswerEvent" "eventType" a_km1_ch1_q2_aNo1

instance ToSchema EditAnswerEvent where
  declareNamedSchema = simpleToSchema'' "_editAnswerEvent" "eventType" e_km1_ch1_q2_aYes1

instance ToSchema DeleteAnswerEvent where
  declareNamedSchema = simpleToSchema'' "_deleteAnswerEvent" "eventType" d_km1_ch1_q2_aYes1
