module Shared.Api.Resource.Event.PhaseEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.PhaseEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Phase.PhaseEvent
import Shared.Util.Swagger

instance ToSchema AddPhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_mtrF

instance ToSchema EditPhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_mtrF

instance ToSchema DeletePhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_mtrF
