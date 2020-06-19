module Shared.Api.Resource.Event.IntegrationEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Util.JSON

instance FromJSON AddIntegrationEvent where
  parseJSON = simpleParseJSON "_addIntegrationEvent"

instance ToJSON AddIntegrationEvent where
  toJSON = simpleToJSON' "_addIntegrationEvent" "eventType"

-- --------------------------------------------
instance FromJSON EditIntegrationEvent where
  parseJSON = simpleParseJSON "_editIntegrationEvent"

instance ToJSON EditIntegrationEvent where
  toJSON = simpleToJSON' "_editIntegrationEvent" "eventType"

-- --------------------------------------------
instance FromJSON DeleteIntegrationEvent where
  parseJSON = simpleParseJSON "_deleteIntegrationEvent"

instance ToJSON DeleteIntegrationEvent where
  toJSON = simpleToJSON' "_deleteIntegrationEvent" "eventType"
