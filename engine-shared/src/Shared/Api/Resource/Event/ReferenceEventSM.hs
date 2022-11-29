module Shared.Api.Resource.Event.ReferenceEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.ReferenceEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Util.Swagger

instance ToSchema AddReferenceEvent

instance ToSchema AddResourcePageReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh1'

instance ToSchema AddURLReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh2'

instance ToSchema AddCrossReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh3'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditReferenceEvent

instance ToSchema EditResourcePageReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh1'

instance ToSchema EditURLReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh2'

instance ToSchema EditCrossReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh3'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_rCh2
