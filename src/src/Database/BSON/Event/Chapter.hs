module Database.BSON.Event.Chapter where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Chapter.DeleteChapterEvent

-- -------------------------
-- ADD CHAPTER EVENT--------
-- -------------------------
instance ToBSON AddChapterEvent where
  toBSON event =
    [ "uuid" BSON.=: serializeUUID (event ^. achUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. achKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. achChapterUuid)
    , "title" BSON.=: (event ^. achTitle)
    , "text" BSON.=: (event ^. achText)
    ]

instance FromBSON AddChapterEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    title <- BSON.lookup "title" doc
    text <- BSON.lookup "text" doc
    return
      AddChapterEvent
      { _achUuid = uuid
      , _achKmUuid = kmUuid
      , _achChapterUuid = chapterUuid
      , _achTitle = title
      , _achText = text
      }

-- -------------------------
-- EDIT CHAPTER EVENT-------
-- -------------------------
instance ToBSON EditChapterEvent where
  toBSON event =
    [ "uuid" BSON.=: serializeUUID (event ^. echUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. echKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. echChapterUuid)
    , "title" BSON.=: (event ^. echTitle)
    , "text" BSON.=: (event ^. echText)
    , "questionIds" BSON.=: serializeMaybeUUIDList (event ^. echQuestionIds)
    ]

instance FromBSON EditChapterEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    title <- BSON.lookup "title" doc
    text <- BSON.lookup "text" doc
    let questionIds = deserializeMaybeUUIDList $ BSON.lookup "questionIds" doc
    return
      EditChapterEvent
      { _echUuid = uuid
      , _echKmUuid = kmUuid
      , _echChapterUuid = chapterUuid
      , _echTitle = title
      , _echText = text
      , _echQuestionIds = questionIds
      }

-- -------------------------
-- DELETE CHAPTER EVENT-----
-- -------------------------
instance ToBSON DeleteChapterEvent where
  toBSON event =
    [ "uuid" BSON.=: serializeUUID (event ^. dchUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. dchKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. dchChapterUuid)
    ]

instance FromBSON DeleteChapterEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    return
      DeleteChapterEvent
      {_dchUuid = uuid, _dchKmUuid = kmUuid, _dchChapterUuid = chapterUuid}
