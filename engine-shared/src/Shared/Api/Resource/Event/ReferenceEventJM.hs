module Shared.Api.Resource.Event.ReferenceEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Util.JSON

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
  parseJSON = simpleParseJSON "_addResourcePageReferenceEvent"

instance ToJSON AddResourcePageReferenceEvent where
  toJSON = simpleToJSON'' "_addResourcePageReferenceEvent" [("referenceType", "ResourcePageReference")]

-- --------------------------------------------
instance FromJSON AddURLReferenceEvent where
  parseJSON = simpleParseJSON "_addURLReferenceEvent"

instance ToJSON AddURLReferenceEvent where
  toJSON = simpleToJSON'' "_addURLReferenceEvent" [("referenceType", "URLReference")]

-- --------------------------------------------
instance FromJSON AddCrossReferenceEvent where
  parseJSON = simpleParseJSON "_addCrossReferenceEvent"

instance ToJSON AddCrossReferenceEvent where
  toJSON = simpleToJSON'' "_addCrossReferenceEvent" [("referenceType", "CrossReference")]

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
  parseJSON = simpleParseJSON "_editResourcePageReferenceEvent"

instance ToJSON EditResourcePageReferenceEvent where
  toJSON = simpleToJSON'' "_editResourcePageReferenceEvent" [("referenceType", "ResourcePageReference")]

-- --------------------------------------------
instance FromJSON EditURLReferenceEvent where
  parseJSON = simpleParseJSON "_editURLReferenceEvent"

instance ToJSON EditURLReferenceEvent where
  toJSON = simpleToJSON'' "_editURLReferenceEvent" [("referenceType", "URLReference")]

-- --------------------------------------------
instance FromJSON EditCrossReferenceEvent where
  parseJSON = simpleParseJSON "_editCrossReferenceEvent"

instance ToJSON EditCrossReferenceEvent where
  toJSON = simpleToJSON'' "_editCrossReferenceEvent" [("referenceType", "CrossReference")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteReferenceEvent where
  parseJSON = simpleParseJSON "_deleteReferenceEvent"

instance ToJSON DeleteReferenceEvent where
  toJSON = simpleToJSON' "_deleteReferenceEvent" "eventType"
