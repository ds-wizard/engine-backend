module Shared.Api.Resource.Event.IntegrationEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.IntegrationEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Util.Swagger

instance ToSchema AddIntegrationEvent

instance ToSchema AddApiIntegrationEvent where
  declareNamedSchema = simpleToSchema'' "_addApiIntegrationEvent" "eventType" a_km1_iop'

instance ToSchema AddWidgetIntegrationEvent where
  declareNamedSchema = simpleToSchema'' "_addWidgetIntegrationEvent" "eventType" a_km1_iwp'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditIntegrationEvent

instance ToSchema EditApiIntegrationEvent where
  declareNamedSchema = simpleToSchema'' "_editApiIntegrationEvent" "eventType" e_km1_iop'

instance ToSchema EditWidgetIntegrationEvent where
  declareNamedSchema = simpleToSchema'' "_editWidgetIntegrationEvent" "eventType" e_km1_iwp'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteIntegrationEvent where
  declareNamedSchema = simpleToSchema'' "_deleteIntegrationEvent" "eventType" d_km1_iop
