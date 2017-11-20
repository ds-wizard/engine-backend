module Database.BSON.Event.Reference where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Event.Reference.AddReferenceEvent
import Model.Event.Reference.DeleteReferenceEvent
import Model.Event.Reference.EditReferenceEvent

-- -------------------------
-- ADD REFERNCE EVENT ------
-- -------------------------
instance ToBSON AddReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. arefUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. arefKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. arefChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. arefQuestionUuid)
    , "referenceUuid" BSON.=: serializeUUID (event ^. arefReferenceUuid)
    , "chapter" BSON.=: (event ^. arefChapter)
    ]

instance FromBSON AddReferenceEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    referenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    chapter <- BSON.lookup "chapter" doc
    return
      AddReferenceEvent
      { _arefUuid = uuid
      , _arefKmUuid = kmUuid
      , _arefChapterUuid = chapterUuid
      , _arefQuestionUuid = questionUuid
      , _arefReferenceUuid = referenceUuid
      , _arefChapter = chapter
      }

-- -------------------------
-- EDIT REFERNCE EVENT -----
-- -------------------------
instance ToBSON EditReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. erefUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. erefKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. erefChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. erefQuestionUuid)
    , "referenceUuid" BSON.=: serializeUUID (event ^. erefReferenceUuid)
    , "chapter" BSON.=: (event ^. erefChapter)
    ]

instance FromBSON EditReferenceEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    referenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    chapter <- BSON.lookup "chapter" doc
    return
      EditReferenceEvent
      { _erefUuid = uuid
      , _erefKmUuid = kmUuid
      , _erefChapterUuid = chapterUuid
      , _erefQuestionUuid = questionUuid
      , _erefReferenceUuid = referenceUuid
      , _erefChapter = chapter
      }

-- -------------------------
-- DELETE REFERNCE EVENT ---
-- -------------------------
instance ToBSON DeleteReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. drefUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. drefKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. drefChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. drefQuestionUuid)
    , "referenceUuid" BSON.=: serializeUUID (event ^. drefReferenceUuid)
    ]

instance FromBSON DeleteReferenceEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    referenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    return
      DeleteReferenceEvent
      { _drefUuid = uuid
      , _drefKmUuid = kmUuid
      , _drefChapterUuid = chapterUuid
      , _drefQuestionUuid = questionUuid
      , _drefReferenceUuid = referenceUuid
      }
