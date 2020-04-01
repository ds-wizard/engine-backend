module Shared.Api.Resource.Event.ReferenceEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.ReferenceEventDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON', simpleToJSON'', toSumJSON)

instance ToJSON AddReferenceEventDTO where
  toJSON = toSumJSON

instance FromJSON AddReferenceEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (AddResourcePageReferenceEventDTO' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (AddURLReferenceEventDTO' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (AddCrossReferenceEventDTO' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddResourcePageReferenceEventDTO where
  parseJSON = simpleParseJSON "_addResourcePageReferenceEventDTO"

instance ToJSON AddResourcePageReferenceEventDTO where
  toJSON = simpleToJSON'' "_addResourcePageReferenceEventDTO" [("referenceType", "ResourcePageReference")]

-- --------------------------------------------
instance FromJSON AddURLReferenceEventDTO where
  parseJSON = simpleParseJSON "_addURLReferenceEventDTO"

instance ToJSON AddURLReferenceEventDTO where
  toJSON = simpleToJSON'' "_addURLReferenceEventDTO" [("referenceType", "URLReference")]

-- --------------------------------------------
instance FromJSON AddCrossReferenceEventDTO where
  parseJSON = simpleParseJSON "_addCrossReferenceEventDTO"

instance ToJSON AddCrossReferenceEventDTO where
  toJSON = simpleToJSON'' "_addCrossReferenceEventDTO" [("referenceType", "CrossReference")]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditReferenceEventDTO where
  toJSON = toSumJSON

instance FromJSON EditReferenceEventDTO where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (EditResourcePageReferenceEventDTO' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (EditURLReferenceEventDTO' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (EditCrossReferenceEventDTO' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditResourcePageReferenceEventDTO where
  parseJSON = simpleParseJSON "_editResourcePageReferenceEventDTO"

instance ToJSON EditResourcePageReferenceEventDTO where
  toJSON = simpleToJSON'' "_editResourcePageReferenceEventDTO" [("referenceType", "ResourcePageReference")]

-- --------------------------------------------
instance FromJSON EditURLReferenceEventDTO where
  parseJSON = simpleParseJSON "_editURLReferenceEventDTO"

instance ToJSON EditURLReferenceEventDTO where
  toJSON = simpleToJSON'' "_editURLReferenceEventDTO" [("referenceType", "URLReference")]

-- --------------------------------------------
instance FromJSON EditCrossReferenceEventDTO where
  parseJSON = simpleParseJSON "_editCrossReferenceEventDTO"

instance ToJSON EditCrossReferenceEventDTO where
  toJSON = simpleToJSON'' "_editCrossReferenceEventDTO" [("referenceType", "CrossReference")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteReferenceEventDTO where
  parseJSON = simpleParseJSON "_deleteReferenceEventDTO"

instance ToJSON DeleteReferenceEventDTO where
  toJSON = simpleToJSON' "_deleteReferenceEventDTO" "eventType"
