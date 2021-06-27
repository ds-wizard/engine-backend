module Wizard.Metamodel.Event.Version3.Reference where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version3.Common

-- ReferenceEventDTO
data AddReferenceEventDTO
  = AddResourcePageReferenceEventDTO' AddResourcePageReferenceEventDTO
  | AddURLReferenceEventDTO' AddURLReferenceEventDTO
  | AddCrossReferenceEventDTO' AddCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

data AddResourcePageReferenceEventDTO =
  AddResourcePageReferenceEventDTO
    { _addResourcePageReferenceEventDTOUuid :: U.UUID
    , _addResourcePageReferenceEventDTOPath :: EventPathDTO
    , _addResourcePageReferenceEventDTOReferenceUuid :: U.UUID
    , _addResourcePageReferenceEventDTOShortUuid :: String
    }
  deriving (Show, Eq, Generic)

data AddURLReferenceEventDTO =
  AddURLReferenceEventDTO
    { _addURLReferenceEventDTOUuid :: U.UUID
    , _addURLReferenceEventDTOPath :: EventPathDTO
    , _addURLReferenceEventDTOReferenceUuid :: U.UUID
    , _addURLReferenceEventDTOUrl :: String
    , _addURLReferenceEventDTOLabel :: String
    }
  deriving (Show, Eq, Generic)

data AddCrossReferenceEventDTO =
  AddCrossReferenceEventDTO
    { _addCrossReferenceEventDTOUuid :: U.UUID
    , _addCrossReferenceEventDTOPath :: EventPathDTO
    , _addCrossReferenceEventDTOReferenceUuid :: U.UUID
    , _addCrossReferenceEventDTOTargetUuid :: U.UUID
    , _addCrossReferenceEventDTODescription :: String
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditReferenceEventDTO
  = EditResourcePageReferenceEventDTO' EditResourcePageReferenceEventDTO
  | EditURLReferenceEventDTO' EditURLReferenceEventDTO
  | EditCrossReferenceEventDTO' EditCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

data EditResourcePageReferenceEventDTO =
  EditResourcePageReferenceEventDTO
    { _editResourcePageReferenceEventDTOUuid :: U.UUID
    , _editResourcePageReferenceEventDTOPath :: EventPathDTO
    , _editResourcePageReferenceEventDTOReferenceUuid :: U.UUID
    , _editResourcePageReferenceEventDTOShortUuid :: EventFieldDTO String
    }
  deriving (Show, Eq, Generic)

data EditURLReferenceEventDTO =
  EditURLReferenceEventDTO
    { _editURLReferenceEventDTOUuid :: U.UUID
    , _editURLReferenceEventDTOPath :: EventPathDTO
    , _editURLReferenceEventDTOReferenceUuid :: U.UUID
    , _editURLReferenceEventDTOUrl :: EventFieldDTO String
    , _editURLReferenceEventDTOLabel :: EventFieldDTO String
    }
  deriving (Show, Eq, Generic)

data EditCrossReferenceEventDTO =
  EditCrossReferenceEventDTO
    { _editCrossReferenceEventDTOUuid :: U.UUID
    , _editCrossReferenceEventDTOPath :: EventPathDTO
    , _editCrossReferenceEventDTOReferenceUuid :: U.UUID
    , _editCrossReferenceEventDTOTargetUuid :: EventFieldDTO U.UUID
    , _editCrossReferenceEventDTODescription :: EventFieldDTO String
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteReferenceEventDTO =
  DeleteReferenceEventDTO
    { _deleteReferenceEventDTOUuid :: U.UUID
    , _deleteReferenceEventDTOPath :: EventPathDTO
    , _deleteReferenceEventDTOReferenceUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- ReferenceEventJM
instance ToJSON AddReferenceEventDTO where
  toJSON (AddResourcePageReferenceEventDTO' event) = toJSON event
  toJSON (AddURLReferenceEventDTO' event) = toJSON event
  toJSON (AddCrossReferenceEventDTO' event) = toJSON event

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
  toJSON AddResourcePageReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "referenceType" .= "ResourcePageReference"
      , "uuid" .= _addResourcePageReferenceEventDTOUuid
      , "path" .= _addResourcePageReferenceEventDTOPath
      , "referenceUuid" .= _addResourcePageReferenceEventDTOReferenceUuid
      , "shortUuid" .= _addResourcePageReferenceEventDTOShortUuid
      ]

-- --------------------------------------------
instance FromJSON AddURLReferenceEventDTO where
  parseJSON = simpleParseJSON "_addURLReferenceEventDTO"

instance ToJSON AddURLReferenceEventDTO where
  toJSON AddURLReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "referenceType" .= "URLReference"
      , "uuid" .= _addURLReferenceEventDTOUuid
      , "path" .= _addURLReferenceEventDTOPath
      , "referenceUuid" .= _addURLReferenceEventDTOReferenceUuid
      , "url" .= _addURLReferenceEventDTOUrl
      , "label" .= _addURLReferenceEventDTOLabel
      ]

-- --------------------------------------------
instance FromJSON AddCrossReferenceEventDTO where
  parseJSON = simpleParseJSON "_addCrossReferenceEventDTO"

instance ToJSON AddCrossReferenceEventDTO where
  toJSON AddCrossReferenceEventDTO {..} =
    object
      [ "eventType" .= "AddReferenceEvent"
      , "referenceType" .= "CrossReference"
      , "uuid" .= _addCrossReferenceEventDTOUuid
      , "path" .= _addCrossReferenceEventDTOPath
      , "referenceUuid" .= _addCrossReferenceEventDTOReferenceUuid
      , "targetUuid" .= _addCrossReferenceEventDTOTargetUuid
      , "description" .= _addCrossReferenceEventDTODescription
      ]

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditReferenceEventDTO where
  toJSON (EditResourcePageReferenceEventDTO' event) = toJSON event
  toJSON (EditURLReferenceEventDTO' event) = toJSON event
  toJSON (EditCrossReferenceEventDTO' event) = toJSON event

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
  toJSON EditResourcePageReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "referenceType" .= "ResourcePageReference"
      , "uuid" .= _editResourcePageReferenceEventDTOUuid
      , "path" .= _editResourcePageReferenceEventDTOPath
      , "referenceUuid" .= _editResourcePageReferenceEventDTOReferenceUuid
      , "shortUuid" .= _editResourcePageReferenceEventDTOShortUuid
      ]

-- --------------------------------------------
instance FromJSON EditURLReferenceEventDTO where
  parseJSON = simpleParseJSON "_editURLReferenceEventDTO"

instance ToJSON EditURLReferenceEventDTO where
  toJSON EditURLReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "referenceType" .= "URLReference"
      , "uuid" .= _editURLReferenceEventDTOUuid
      , "path" .= _editURLReferenceEventDTOPath
      , "referenceUuid" .= _editURLReferenceEventDTOReferenceUuid
      , "url" .= _editURLReferenceEventDTOUrl
      , "label" .= _editURLReferenceEventDTOLabel
      ]

-- --------------------------------------------
instance FromJSON EditCrossReferenceEventDTO where
  parseJSON = simpleParseJSON "_editCrossReferenceEventDTO"

instance ToJSON EditCrossReferenceEventDTO where
  toJSON EditCrossReferenceEventDTO {..} =
    object
      [ "eventType" .= "EditReferenceEvent"
      , "referenceType" .= "CrossReference"
      , "uuid" .= _editCrossReferenceEventDTOUuid
      , "path" .= _editCrossReferenceEventDTOPath
      , "referenceUuid" .= _editCrossReferenceEventDTOReferenceUuid
      , "targetUuid" .= _editCrossReferenceEventDTOTargetUuid
      , "description" .= _editCrossReferenceEventDTODescription
      ]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON DeleteReferenceEventDTO where
  parseJSON = simpleParseJSON "_deleteReferenceEventDTO"

instance ToJSON DeleteReferenceEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteReferenceEventDTO"
