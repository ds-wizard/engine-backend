module Shared.Api.Resource.Event.ReferenceEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Util.Aeson

instance ToJSON AddReferenceEvent where
  toJSON = toSumJSON

instance FromJSON AddReferenceEvent where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (AddResourcePageReferenceEvent' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (AddURLReferenceEvent' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (AddCrossReferenceEvent' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddResourcePageReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddResourcePageReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "ResourcePageReference")]

-- --------------------------------------------
instance FromJSON AddURLReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddURLReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "URLReference")]

-- --------------------------------------------
instance FromJSON AddCrossReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddCrossReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "CrossReference")]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditReferenceEvent where
  toJSON = toSumJSON

instance FromJSON EditReferenceEvent where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (EditResourcePageReferenceEvent' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (EditURLReferenceEvent' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (EditCrossReferenceEvent' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditResourcePageReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditResourcePageReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "ResourcePageReference")]

-- --------------------------------------------
instance FromJSON EditURLReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditURLReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "URLReference")]

-- --------------------------------------------
instance FromJSON EditCrossReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditCrossReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "CrossReference")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteReferenceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
