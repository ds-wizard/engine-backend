module Shared.Api.Resource.Event.IntegrationEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.IntegrationEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Util.Swagger

instance ToSchema AddIntegrationEvent where
  declareNamedSchema = simpleToSchema'' "_addIntegrationEvent" "eventType" a_km1_iop

instance ToSchema EditIntegrationEvent where
  declareNamedSchema = simpleToSchema'' "_editIntegrationEvent" "eventType" e_km1_iop

instance ToSchema DeleteIntegrationEvent where
  declareNamedSchema = simpleToSchema'' "_deleteIntegrationEvent" "eventType" d_km1_iop
