module Shared.Api.Resource.Event.IntegrationEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.IntegrationEventDTO
import Shared.Api.Resource.Event.IntegrationEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema AddIntegrationEventDTO where
  declareNamedSchema = simpleToSchema'' "_addIntegrationEventDTO" "eventType" (toDTO a_km1_iop)

instance ToSchema EditIntegrationEventDTO where
  declareNamedSchema = simpleToSchema'' "_editIntegrationEventDTO" "eventType" (toDTO e_km1_iop)

instance ToSchema DeleteIntegrationEventDTO where
  declareNamedSchema = simpleToSchema'' "_deleteIntegrationEventDTO" "eventType" (toDTO d_km1_iop)
