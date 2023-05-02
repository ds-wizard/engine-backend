module WizardLib.KnowledgeModel.Api.Resource.Event.IntegrationEventJM where

import Control.Monad
import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent

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
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddApiIntegrationEvent where
  toJSON = toJSONWithAdditionalData [("integrationType", "ApiIntegration")]

-- --------------------------------------------
instance FromJSON AddWidgetIntegrationEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddWidgetIntegrationEvent where
  toJSON = toJSONWithAdditionalData [("integrationType", "WidgetIntegration")]

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
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditApiIntegrationEvent where
  toJSON = toJSONWithAdditionalData [("integrationType", "ApiIntegration")]

-- --------------------------------------------
instance FromJSON EditWidgetIntegrationEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditWidgetIntegrationEvent where
  toJSON = toJSONWithAdditionalData [("integrationType", "WidgetIntegration")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteIntegrationEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteIntegrationEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
