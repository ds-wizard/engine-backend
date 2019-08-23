module Database.BSON.Event.Tag where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Model.Event.Tag.TagEvent

-- -------------------------
-- ADD TAG EVENT -----------
-- -------------------------
instance ToBSON AddTagEvent where
  toBSON AddTagEvent {..} =
    [ "eventType" BSON.=: "AddTagEvent"
    , "uuid" BSON.=: _addTagEventUuid
    , "parentUuid" BSON.=: _addTagEventParentUuid
    , "entityUuid" BSON.=: _addTagEventEntityUuid
    , "name" BSON.=: _addTagEventName
    , "description" BSON.=: _addTagEventDescription
    , "color" BSON.=: _addTagEventColor
    ]

instance FromBSON AddTagEvent where
  fromBSON doc = do
    _addTagEventUuid <- BSON.lookup "uuid" doc
    _addTagEventParentUuid <- BSON.lookup "parentUuid" doc
    _addTagEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addTagEventName <- BSON.lookup "name" doc
    _addTagEventDescription <- BSON.lookup "description" doc
    _addTagEventColor <- BSON.lookup "color" doc
    return AddTagEvent {..}

-- -------------------------
-- EDIT TAG EVENT ----------
-- -------------------------
instance ToBSON EditTagEvent where
  toBSON EditTagEvent {..} =
    [ "eventType" BSON.=: "EditTagEvent"
    , "uuid" BSON.=: _editTagEventUuid
    , "parentUuid" BSON.=: _editTagEventParentUuid
    , "entityUuid" BSON.=: _editTagEventEntityUuid
    , "name" BSON.=: _editTagEventName
    , "description" BSON.=: _editTagEventDescription
    , "color" BSON.=: _editTagEventColor
    ]

instance FromBSON EditTagEvent where
  fromBSON doc = do
    _editTagEventUuid <- BSON.lookup "uuid" doc
    _editTagEventParentUuid <- BSON.lookup "parentUuid" doc
    _editTagEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editTagEventName <- BSON.lookup "name" doc
    _editTagEventDescription <- BSON.lookup "description" doc
    _editTagEventColor <- BSON.lookup "color" doc
    return EditTagEvent {..}

-- -------------------------
-- DELETE TAG EVENT --------
-- -------------------------
instance ToBSON DeleteTagEvent where
  toBSON DeleteTagEvent {..} =
    [ "eventType" BSON.=: "DeleteTagEvent"
    , "uuid" BSON.=: _deleteTagEventUuid
    , "parentUuid" BSON.=: _deleteTagEventParentUuid
    , "entityUuid" BSON.=: _deleteTagEventEntityUuid
    ]

instance FromBSON DeleteTagEvent where
  fromBSON doc = do
    _deleteTagEventUuid <- BSON.lookup "uuid" doc
    _deleteTagEventParentUuid <- BSON.lookup "parentUuid" doc
    _deleteTagEventEntityUuid <- BSON.lookup "entityUuid" doc
    return DeleteTagEvent {..}
