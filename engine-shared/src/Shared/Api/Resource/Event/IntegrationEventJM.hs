module Shared.Api.Resource.Event.IntegrationEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.IntegrationEventDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddIntegrationEventDTO where
  parseJSON = simpleParseJSON "_addIntegrationEventDTO"

instance ToJSON AddIntegrationEventDTO where
  toJSON = simpleToJSON' "_addIntegrationEventDTO" "eventType"

-- --------------------------------------------
instance FromJSON EditIntegrationEventDTO where
  parseJSON = simpleParseJSON "_editIntegrationEventDTO"

instance ToJSON EditIntegrationEventDTO where
  toJSON = simpleToJSON' "_editIntegrationEventDTO" "eventType"

-- --------------------------------------------
instance FromJSON DeleteIntegrationEventDTO where
  parseJSON = simpleParseJSON "_deleteIntegrationEventDTO"

instance ToJSON DeleteIntegrationEventDTO where
  toJSON = simpleToJSON' "_deleteIntegrationEventDTO" "eventType"
