module Shared.Api.Resource.Event.IntegrationEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Util.JSON

instance ToJSON AddIntegrationEvent where
  toJSON = toSumJSON

instance FromJSON AddIntegrationEvent where
  parseJSON (Object o) = do
    integrationType <- o .: "integrationType"
    case integrationType of
      "ApiIntegration" -> parseJSON (Object o) >>= \event -> return (AddApiIntegrationEvent' event)
      "WidgetIntegration" -> parseJSON (Object o) >>= \event -> return (AddWidgetIntegrationEvent' event)
      _ -> fail "One of the events has unsupported integrationType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddApiIntegrationEvent where
  parseJSON = simpleParseJSON "_addApiIntegrationEvent"

instance ToJSON AddApiIntegrationEvent where
  toJSON = simpleToJSON'' "_addApiIntegrationEvent" [("integrationType", "ApiIntegration")]

-- --------------------------------------------
instance FromJSON AddWidgetIntegrationEvent where
  parseJSON = simpleParseJSON "_addWidgetIntegrationEvent"

instance ToJSON AddWidgetIntegrationEvent where
  toJSON = simpleToJSON'' "_addWidgetIntegrationEvent" [("integrationType", "WidgetIntegration")]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditIntegrationEvent where
  toJSON = toSumJSON

instance FromJSON EditIntegrationEvent where
  parseJSON (Object o) = do
    integrationType <- o .: "integrationType"
    case integrationType of
      "ApiIntegration" -> parseJSON (Object o) >>= \event -> return (EditApiIntegrationEvent' event)
      "WidgetIntegration" -> parseJSON (Object o) >>= \event -> return (EditWidgetIntegrationEvent' event)
      _ -> fail "One of the events has unsupported integrationType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditApiIntegrationEvent where
  parseJSON = simpleParseJSON "_editApiIntegrationEvent"

instance ToJSON EditApiIntegrationEvent where
  toJSON = simpleToJSON'' "_editApiIntegrationEvent" [("integrationType", "ApiIntegration")]

-- --------------------------------------------
instance FromJSON EditWidgetIntegrationEvent where
  parseJSON = simpleParseJSON "_editWidgetIntegrationEvent"

instance ToJSON EditWidgetIntegrationEvent where
  toJSON = simpleToJSON'' "_editWidgetIntegrationEvent" [("integrationType", "WidgetIntegration")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteIntegrationEvent where
  parseJSON = simpleParseJSON "_deleteIntegrationEvent"

instance ToJSON DeleteIntegrationEvent where
  toJSON = simpleToJSON' "_deleteIntegrationEvent" "eventType"
