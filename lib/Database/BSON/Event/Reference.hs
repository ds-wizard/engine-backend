module Database.BSON.Event.Reference where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Model.Event.Reference.ReferenceEvent

-- -------------------------
-- ADD REFERNCE EVENT ------
-- -------------------------
instance ToBSON AddReferenceEvent where
  toBSON (AddResourcePageReferenceEvent' event) = toBSON event
  toBSON (AddURLReferenceEvent' event) = toBSON event
  toBSON (AddCrossReferenceEvent' event) = toBSON event

instance FromBSON AddReferenceEvent where
  fromBSON doc = do
    referenceType <- BSON.lookup "referenceType" doc
    case referenceType of
      "ResourcePageReference" ->
        AddResourcePageReferenceEvent' <$> (fromBSON doc :: Maybe AddResourcePageReferenceEvent)
      "URLReference" -> AddURLReferenceEvent' <$> (fromBSON doc :: Maybe AddURLReferenceEvent)
      "CrossReference" -> AddCrossReferenceEvent' <$> (fromBSON doc :: Maybe AddCrossReferenceEvent)

-- ------------------------------------------------
instance ToBSON AddResourcePageReferenceEvent where
  toBSON AddResourcePageReferenceEvent {..} =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "referenceType" BSON.=: "ResourcePageReference"
    , "uuid" BSON.=: _addResourcePageReferenceEventUuid
    , "parentUuid" BSON.=: _addResourcePageReferenceEventParentUuid
    , "entityUuid" BSON.=: _addResourcePageReferenceEventEntityUuid
    , "shortUuid" BSON.=: _addResourcePageReferenceEventShortUuid
    ]

instance FromBSON AddResourcePageReferenceEvent where
  fromBSON doc = do
    _addResourcePageReferenceEventUuid <- BSON.lookup "uuid" doc
    _addResourcePageReferenceEventParentUuid <- BSON.lookup "parentUuid" doc
    _addResourcePageReferenceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addResourcePageReferenceEventShortUuid <- BSON.lookup "shortUuid" doc
    return AddResourcePageReferenceEvent {..}

-- ------------------------------------------------
instance ToBSON AddURLReferenceEvent where
  toBSON AddURLReferenceEvent {..} =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "referenceType" BSON.=: "URLReference"
    , "uuid" BSON.=: _addURLReferenceEventUuid
    , "parentUuid" BSON.=: _addURLReferenceEventParentUuid
    , "entityUuid" BSON.=: _addURLReferenceEventEntityUuid
    , "url" BSON.=: _addURLReferenceEventUrl
    , "label" BSON.=: _addURLReferenceEventLabel
    ]

instance FromBSON AddURLReferenceEvent where
  fromBSON doc = do
    _addURLReferenceEventUuid <- BSON.lookup "uuid" doc
    _addURLReferenceEventParentUuid <- BSON.lookup "parentUuid" doc
    _addURLReferenceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addURLReferenceEventUrl <- BSON.lookup "url" doc
    _addURLReferenceEventLabel <- BSON.lookup "label" doc
    return AddURLReferenceEvent {..}

-- ------------------------------------------------
instance ToBSON AddCrossReferenceEvent where
  toBSON AddCrossReferenceEvent {..} =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "referenceType" BSON.=: "CrossReference"
    , "uuid" BSON.=: _addCrossReferenceEventUuid
    , "parentUuid" BSON.=: _addCrossReferenceEventParentUuid
    , "entityUuid" BSON.=: _addCrossReferenceEventEntityUuid
    , "targetUuid" BSON.=: _addCrossReferenceEventTargetUuid
    , "description" BSON.=: _addCrossReferenceEventDescription
    ]

instance FromBSON AddCrossReferenceEvent where
  fromBSON doc = do
    _addCrossReferenceEventUuid <- BSON.lookup "uuid" doc
    _addCrossReferenceEventParentUuid <- BSON.lookup "parentUuid" doc
    _addCrossReferenceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addCrossReferenceEventTargetUuid <- BSON.lookup "targetUuid" doc
    _addCrossReferenceEventDescription <- BSON.lookup "description" doc
    return AddCrossReferenceEvent {..}

-- -------------------------
-- EDIT REFERNCE EVENT -----
-- -------------------------
instance ToBSON EditReferenceEvent where
  toBSON (EditResourcePageReferenceEvent' event) = toBSON event
  toBSON (EditURLReferenceEvent' event) = toBSON event
  toBSON (EditCrossReferenceEvent' event) = toBSON event

instance FromBSON EditReferenceEvent where
  fromBSON doc = do
    referenceType <- BSON.lookup "referenceType" doc
    case referenceType of
      "ResourcePageReference" ->
        EditResourcePageReferenceEvent' <$> (fromBSON doc :: Maybe EditResourcePageReferenceEvent)
      "URLReference" -> EditURLReferenceEvent' <$> (fromBSON doc :: Maybe EditURLReferenceEvent)
      "CrossReference" -> EditCrossReferenceEvent' <$> (fromBSON doc :: Maybe EditCrossReferenceEvent)

-- ------------------------------------------------
instance ToBSON EditResourcePageReferenceEvent where
  toBSON EditResourcePageReferenceEvent {..} =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "referenceType" BSON.=: "ResourcePageReference"
    , "uuid" BSON.=: _editResourcePageReferenceEventUuid
    , "parentUuid" BSON.=: _editResourcePageReferenceEventParentUuid
    , "entityUuid" BSON.=: _editResourcePageReferenceEventEntityUuid
    , "shortUuid" BSON.=: _editResourcePageReferenceEventShortUuid
    ]

instance FromBSON EditResourcePageReferenceEvent where
  fromBSON doc = do
    _editResourcePageReferenceEventUuid <- BSON.lookup "uuid" doc
    _editResourcePageReferenceEventParentUuid <- BSON.lookup "parentUuid" doc
    _editResourcePageReferenceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editResourcePageReferenceEventShortUuid <- BSON.lookup "shortUuid" doc
    return EditResourcePageReferenceEvent {..}

-- ------------------------------------------------
instance ToBSON EditURLReferenceEvent where
  toBSON EditURLReferenceEvent {..} =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "referenceType" BSON.=: "URLReference"
    , "uuid" BSON.=: _editURLReferenceEventUuid
    , "parentUuid" BSON.=: _editURLReferenceEventParentUuid
    , "entityUuid" BSON.=: _editURLReferenceEventEntityUuid
    , "url" BSON.=: _editURLReferenceEventUrl
    , "label" BSON.=: _editURLReferenceEventLabel
    ]

instance FromBSON EditURLReferenceEvent where
  fromBSON doc = do
    _editURLReferenceEventUuid <- BSON.lookup "uuid" doc
    _editURLReferenceEventParentUuid <- BSON.lookup "parentUuid" doc
    _editURLReferenceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editURLReferenceEventUrl <- BSON.lookup "url" doc
    _editURLReferenceEventLabel <- BSON.lookup "label" doc
    return EditURLReferenceEvent {..}

-- ------------------------------------------------
instance ToBSON EditCrossReferenceEvent where
  toBSON EditCrossReferenceEvent {..} =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "referenceType" BSON.=: "CrossReference"
    , "uuid" BSON.=: _editCrossReferenceEventUuid
    , "parentUuid" BSON.=: _editCrossReferenceEventParentUuid
    , "entityUuid" BSON.=: _editCrossReferenceEventEntityUuid
    , "targetUuid" BSON.=: _editCrossReferenceEventTargetUuid
    , "description" BSON.=: _editCrossReferenceEventDescription
    ]

instance FromBSON EditCrossReferenceEvent where
  fromBSON doc = do
    _editCrossReferenceEventUuid <- BSON.lookup "uuid" doc
    _editCrossReferenceEventParentUuid <- BSON.lookup "parentUuid" doc
    _editCrossReferenceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editCrossReferenceEventTargetUuid <- BSON.lookup "targetUuid" doc
    _editCrossReferenceEventDescription <- BSON.lookup "description" doc
    return EditCrossReferenceEvent {..}

-- -------------------------
-- DELETE REFERNCE EVENT ---
-- -------------------------
instance ToBSON DeleteReferenceEvent where
  toBSON DeleteReferenceEvent {..} =
    [ "eventType" BSON.=: "DeleteReferenceEvent"
    , "uuid" BSON.=: _deleteReferenceEventUuid
    , "parentUuid" BSON.=: _deleteReferenceEventParentUuid
    , "entityUuid" BSON.=: _deleteReferenceEventEntityUuid
    ]

instance FromBSON DeleteReferenceEvent where
  fromBSON doc = do
    _deleteReferenceEventUuid <- BSON.lookup "uuid" doc
    _deleteReferenceEventParentUuid <- BSON.lookup "parentUuid" doc
    _deleteReferenceEventEntityUuid <- BSON.lookup "entityUuid" doc
    return DeleteReferenceEvent {..}
