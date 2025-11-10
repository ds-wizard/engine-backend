module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Resource.ResourceEvent

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
  parseJSON _ = pure DeleteResourceCollectionEvent

instance ToJSON DeleteResourceCollectionEvent where
  toJSON _ = toJSON [("eventType", String "DeleteResourceCollectionEvent")]

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
