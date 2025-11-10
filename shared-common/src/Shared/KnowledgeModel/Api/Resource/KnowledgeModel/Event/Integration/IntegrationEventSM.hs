module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Integration.IntegrationEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Integration.IntegrationEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent

instance ToSchema AddIntegrationEvent

instance ToSchema AddApiIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ir

instance ToSchema AddApiLegacyIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_iop

instance ToSchema AddWidgetIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_iwp

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditIntegrationEvent

instance ToSchema EditApiIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ir

instance ToSchema EditApiLegacyIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_iop

instance ToSchema EditWidgetIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_iwp

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_iop
