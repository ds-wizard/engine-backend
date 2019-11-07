module Database.BSON.Event.Chapter where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Model.Event.Chapter.ChapterEvent

-- -------------------------
-- ADD CHAPTER EVENT--------
-- -------------------------
instance ToBSON AddChapterEvent where
  toBSON AddChapterEvent {..} =
    [ "eventType" BSON.=: "AddChapterEvent"
    , "uuid" BSON.=: _addChapterEventUuid
    , "parentUuid" BSON.=: _addChapterEventParentUuid
    , "entityUuid" BSON.=: _addChapterEventEntityUuid
    , "title" BSON.=: _addChapterEventTitle
    , "text" BSON.=: _addChapterEventText
    ]

instance FromBSON AddChapterEvent where
  fromBSON doc = do
    _addChapterEventUuid <- BSON.lookup "uuid" doc
    _addChapterEventParentUuid <- BSON.lookup "parentUuid" doc
    _addChapterEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addChapterEventTitle <- BSON.lookup "title" doc
    _addChapterEventText <- BSON.lookup "text" doc
    return AddChapterEvent {..}

-- -------------------------
-- EDIT CHAPTER EVENT-------
-- -------------------------
instance ToBSON EditChapterEvent where
  toBSON EditChapterEvent {..} =
    [ "eventType" BSON.=: "EditChapterEvent"
    , "uuid" BSON.=: _editChapterEventUuid
    , "parentUuid" BSON.=: _editChapterEventParentUuid
    , "entityUuid" BSON.=: _editChapterEventEntityUuid
    , "title" BSON.=: _editChapterEventTitle
    , "text" BSON.=: _editChapterEventText
    , "questionUuids" BSON.=: _editChapterEventQuestionUuids
    ]

instance FromBSON EditChapterEvent where
  fromBSON doc = do
    _editChapterEventUuid <- BSON.lookup "uuid" doc
    _editChapterEventParentUuid <- BSON.lookup "parentUuid" doc
    _editChapterEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editChapterEventTitle <- BSON.lookup "title" doc
    _editChapterEventText <- BSON.lookup "text" doc
    _editChapterEventQuestionUuids <- BSON.lookup "questionUuids" doc
    return EditChapterEvent {..}

-- -------------------------
-- DELETE CHAPTER EVENT-----
-- -------------------------
instance ToBSON DeleteChapterEvent where
  toBSON DeleteChapterEvent {..} =
    [ "eventType" BSON.=: "DeleteChapterEvent"
    , "uuid" BSON.=: _deleteChapterEventUuid
    , "parentUuid" BSON.=: _deleteChapterEventParentUuid
    , "entityUuid" BSON.=: _deleteChapterEventEntityUuid
    ]

instance FromBSON DeleteChapterEvent where
  fromBSON doc = do
    _deleteChapterEventUuid <- BSON.lookup "uuid" doc
    _deleteChapterEventParentUuid <- BSON.lookup "parentUuid" doc
    _deleteChapterEventEntityUuid <- BSON.lookup "entityUuid" doc
    return DeleteChapterEvent {..}
