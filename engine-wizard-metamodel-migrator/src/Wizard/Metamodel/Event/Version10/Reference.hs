module Wizard.Metamodel.Event.Version10.Reference where

import Control.Monad
import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version10.Common

-- Shared.Model.Event.Reference.ReferenceEvent
data AddReferenceEvent
  = AddResourcePageReferenceEvent' AddResourcePageReferenceEvent
  | AddURLReferenceEvent' AddURLReferenceEvent
  | AddCrossReferenceEvent' AddCrossReferenceEvent
  deriving (Show, Eq, Generic)

data AddResourcePageReferenceEvent =
  AddResourcePageReferenceEvent
    { _addResourcePageReferenceEventUuid :: U.UUID
    , _addResourcePageReferenceEventParentUuid :: U.UUID
    , _addResourcePageReferenceEventEntityUuid :: U.UUID
    , _addResourcePageReferenceEventShortUuid :: String
    , _addResourcePageReferenceEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data AddURLReferenceEvent =
  AddURLReferenceEvent
    { _addURLReferenceEventUuid :: U.UUID
    , _addURLReferenceEventParentUuid :: U.UUID
    , _addURLReferenceEventEntityUuid :: U.UUID
    , _addURLReferenceEventUrl :: String
    , _addURLReferenceEventLabel :: String
    , _addURLReferenceEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data AddCrossReferenceEvent =
  AddCrossReferenceEvent
    { _addCrossReferenceEventUuid :: U.UUID
    , _addCrossReferenceEventParentUuid :: U.UUID
    , _addCrossReferenceEventEntityUuid :: U.UUID
    , _addCrossReferenceEventTargetUuid :: U.UUID
    , _addCrossReferenceEventDescription :: String
    , _addCrossReferenceEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditReferenceEvent
  = EditResourcePageReferenceEvent' EditResourcePageReferenceEvent
  | EditURLReferenceEvent' EditURLReferenceEvent
  | EditCrossReferenceEvent' EditCrossReferenceEvent
  deriving (Show, Eq, Generic)

data EditResourcePageReferenceEvent =
  EditResourcePageReferenceEvent
    { _editResourcePageReferenceEventUuid :: U.UUID
    , _editResourcePageReferenceEventParentUuid :: U.UUID
    , _editResourcePageReferenceEventEntityUuid :: U.UUID
    , _editResourcePageReferenceEventShortUuid :: EventField String
    , _editResourcePageReferenceEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

data EditURLReferenceEvent =
  EditURLReferenceEvent
    { _editURLReferenceEventUuid :: U.UUID
    , _editURLReferenceEventParentUuid :: U.UUID
    , _editURLReferenceEventEntityUuid :: U.UUID
    , _editURLReferenceEventUrl :: EventField String
    , _editURLReferenceEventLabel :: EventField String
    , _editURLReferenceEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

data EditCrossReferenceEvent =
  EditCrossReferenceEvent
    { _editCrossReferenceEventUuid :: U.UUID
    , _editCrossReferenceEventParentUuid :: U.UUID
    , _editCrossReferenceEventEntityUuid :: U.UUID
    , _editCrossReferenceEventTargetUuid :: EventField U.UUID
    , _editCrossReferenceEventDescription :: EventField String
    , _editCrossReferenceEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteReferenceEvent =
  DeleteReferenceEvent
    { _deleteReferenceEventUuid :: U.UUID
    , _deleteReferenceEventParentUuid :: U.UUID
    , _deleteReferenceEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- Shared.Api.Resource.Event.ReferenceEventJM
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
