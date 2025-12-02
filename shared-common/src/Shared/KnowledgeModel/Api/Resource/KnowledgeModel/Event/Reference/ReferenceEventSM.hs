module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Reference.ReferenceEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Reference.ReferenceEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Reference.ReferenceEvent

instance ToSchema AddReferenceEvent

instance ToSchema AddResourcePageReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh1

instance ToSchema AddURLReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh2

instance ToSchema AddCrossReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh3

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditReferenceEvent

instance ToSchema EditResourcePageReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh1

instance ToSchema EditURLReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh2

instance ToSchema EditCrossReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh3

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_rCh2
