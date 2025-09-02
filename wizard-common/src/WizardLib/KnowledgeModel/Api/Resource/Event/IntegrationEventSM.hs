module WizardLib.KnowledgeModel.Api.Resource.Event.IntegrationEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.IntegrationEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent

instance ToSchema AddIntegrationEvent

instance ToSchema AddApiIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ir'

instance ToSchema AddApiLegacyIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_iop'

instance ToSchema AddWidgetIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_iwp'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditIntegrationEvent

instance ToSchema EditApiIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ir'

instance ToSchema EditApiLegacyIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_iop'

instance ToSchema EditWidgetIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_iwp'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_iop
