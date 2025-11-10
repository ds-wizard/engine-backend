module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent

instance ToSchema AddAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_aNo1

instance ToSchema EditAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_aYes1

instance ToSchema DeleteAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_aYes1
