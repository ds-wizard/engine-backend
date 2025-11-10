module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Phase.PhaseEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Phase.PhaseEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Phase.PhaseEvent

instance ToSchema AddPhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_mtrF

instance ToSchema EditPhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_mtrF

instance ToSchema DeletePhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_mtrF
