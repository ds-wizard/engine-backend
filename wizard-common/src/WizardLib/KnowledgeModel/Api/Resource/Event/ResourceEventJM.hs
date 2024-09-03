module WizardLib.KnowledgeModel.Api.Resource.Event.ResourceEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEvent

instance FromJSON AddResourceCollectionEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddResourceCollectionEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditResourceCollectionEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditResourceCollectionEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON DeleteResourceCollectionEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON DeleteResourceCollectionEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON AddResourcePageEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddResourcePageEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditResourcePageEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditResourcePageEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON DeleteResourcePageEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON DeleteResourcePageEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
