module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEvent

instance ToSchema AddExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_eAlbert

instance ToSchema EditExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_eAlbert

instance ToSchema DeleteExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_eNikola
