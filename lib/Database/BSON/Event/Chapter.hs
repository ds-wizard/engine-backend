module Database.BSON.Event.Chapter where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import LensesConfig
import Model.Event.Chapter.ChapterEvent

-- -------------------------
-- ADD CHAPTER EVENT--------
-- -------------------------
instance ToBSON AddChapterEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddChapterEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    ]

instance FromBSON AddChapterEvent where
  fromBSON doc = do
    chUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    chKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    chTitle <- BSON.lookup "title" doc
    chText <- BSON.lookup "text" doc
    return
      AddChapterEvent
      { _addChapterEventUuid = chUuid
      , _addChapterEventKmUuid = chKmUuid
      , _addChapterEventChapterUuid = chChapterUuid
      , _addChapterEventTitle = chTitle
      , _addChapterEventText = chText
      }

-- -------------------------
-- EDIT CHAPTER EVENT-------
-- -------------------------
instance ToBSON EditChapterEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditChapterEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "questionIds" BSON.=: serializeMaybeUUIDList (event ^. questionIds)
    ]

instance FromBSON EditChapterEvent where
  fromBSON doc = do
    chUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    chKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    chTitle <- BSON.lookup "title" doc
    chText <- BSON.lookup "text" doc
    let chQuestionIds = deserializeMaybeUUIDList $ BSON.lookup "questionIds" doc
    return
      EditChapterEvent
      { _editChapterEventUuid = chUuid
      , _editChapterEventKmUuid = chKmUuid
      , _editChapterEventChapterUuid = chChapterUuid
      , _editChapterEventTitle = chTitle
      , _editChapterEventText = chText
      , _editChapterEventQuestionIds = chQuestionIds
      }

-- -------------------------
-- DELETE CHAPTER EVENT-----
-- -------------------------
instance ToBSON DeleteChapterEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteChapterEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    ]

instance FromBSON DeleteChapterEvent where
  fromBSON doc = do
    chUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    chKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    return
      DeleteChapterEvent
      { _deleteChapterEventUuid = chUuid
      , _deleteChapterEventKmUuid = chKmUuid
      , _deleteChapterEventChapterUuid = chChapterUuid
      }
