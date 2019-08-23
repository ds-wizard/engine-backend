module Database.BSON.Event.Expert where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Model.Event.Expert.ExpertEvent

-- -------------------------
-- ADD EXPERT EVENT---------
-- -------------------------
instance ToBSON AddExpertEvent where
  toBSON AddExpertEvent {..} =
    [ "eventType" BSON.=: "AddExpertEvent"
    , "uuid" BSON.=: _addExpertEventUuid
    , "parentUuid" BSON.=: _addExpertEventParentUuid
    , "entityUuid" BSON.=: _addExpertEventEntityUuid
    , "name" BSON.=: _addExpertEventName
    , "email" BSON.=: _addExpertEventEmail
    ]

instance FromBSON AddExpertEvent where
  fromBSON doc = do
    _addExpertEventUuid <- BSON.lookup "uuid" doc
    _addExpertEventParentUuid <- BSON.lookup "parentUuid" doc
    _addExpertEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addExpertEventName <- BSON.lookup "name" doc
    _addExpertEventEmail <- BSON.lookup "email" doc
    return AddExpertEvent {..}

-- -------------------------
-- EDIT EXPERT EVENT--------
-- -------------------------
instance ToBSON EditExpertEvent where
  toBSON EditExpertEvent {..} =
    [ "eventType" BSON.=: "EditExpertEvent"
    , "uuid" BSON.=: _editExpertEventUuid
    , "parentUuid" BSON.=: _editExpertEventParentUuid
    , "entityUuid" BSON.=: _editExpertEventEntityUuid
    , "name" BSON.=: _editExpertEventName
    , "email" BSON.=: _editExpertEventEmail
    ]

instance FromBSON EditExpertEvent where
  fromBSON doc = do
    _editExpertEventUuid <- BSON.lookup "uuid" doc
    _editExpertEventParentUuid <- BSON.lookup "parentUuid" doc
    _editExpertEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editExpertEventName <- BSON.lookup "name" doc
    _editExpertEventEmail <- BSON.lookup "email" doc
    return EditExpertEvent {..}

-- -------------------------
-- DELETE EXPERT EVENT------
-- -------------------------
instance ToBSON DeleteExpertEvent where
  toBSON DeleteExpertEvent {..} =
    [ "eventType" BSON.=: "DeleteExpertEvent"
    , "uuid" BSON.=: _deleteExpertEventUuid
    , "parentUuid" BSON.=: _deleteExpertEventParentUuid
    , "entityUuid" BSON.=: _deleteExpertEventEntityUuid
    ]

instance FromBSON DeleteExpertEvent where
  fromBSON doc = do
    _deleteExpertEventUuid <- BSON.lookup "uuid" doc
    _deleteExpertEventParentUuid <- BSON.lookup "parentUuid" doc
    _deleteExpertEventEntityUuid <- BSON.lookup "entityUuid" doc
    return DeleteExpertEvent {..}
