module Shared.Api.Resource.Event.ExpertEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.ExpertEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Util.Swagger

instance ToSchema AddExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_eAlbert

instance ToSchema EditExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_eAlbert

instance ToSchema DeleteExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_eNikola
