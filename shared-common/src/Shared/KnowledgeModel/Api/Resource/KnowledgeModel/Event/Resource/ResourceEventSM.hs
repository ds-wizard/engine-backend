module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Resource.ResourceEvent

instance ToSchema AddResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_rc1

instance ToSchema EditResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_rc1

instance ToSchema DeleteResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_rc1

-- --------------------------------------------
instance ToSchema AddResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_rc1_rp1

instance ToSchema EditResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_rc1_rp1

instance ToSchema DeleteResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_rc1_rp1
