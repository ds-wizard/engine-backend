module Database.BSON.Event.Reference where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import LensesConfig
import Model.Event.Reference.ReferenceEvent

-- -------------------------
-- ADD REFERNCE EVENT ------
-- -------------------------
instance ToBSON AddReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "referenceUuid" BSON.=: serializeUUID (event ^. referenceUuid)
    , "chapter" BSON.=: (event ^. chapter)
    ]

instance FromBSON AddReferenceEvent where
  fromBSON doc = do
    refUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    refKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    refChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    refQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    refChapter <- BSON.lookup "chapter" doc
    return
      AddReferenceEvent
      { _addReferenceEventUuid = refUuid
      , _addReferenceEventKmUuid = refKmUuid
      , _addReferenceEventChapterUuid = refChapterUuid
      , _addReferenceEventQuestionUuid = refQuestionUuid
      , _addReferenceEventReferenceUuid = refReferenceUuid
      , _addReferenceEventChapter = refChapter
      }

-- -------------------------
-- EDIT REFERNCE EVENT -----
-- -------------------------
instance ToBSON EditReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "referenceUuid" BSON.=: serializeUUID (event ^. referenceUuid)
    , "chapter" BSON.=: (event ^. chapter)
    ]

instance FromBSON EditReferenceEvent where
  fromBSON doc = do
    refUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    refKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    refChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    refQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    refChapter <- BSON.lookup "chapter" doc
    return
      EditReferenceEvent
      { _editReferenceEventUuid = refUuid
      , _editReferenceEventKmUuid = refKmUuid
      , _editReferenceEventChapterUuid = refChapterUuid
      , _editReferenceEventQuestionUuid = refQuestionUuid
      , _editReferenceEventReferenceUuid = refReferenceUuid
      , _editReferenceEventChapter = refChapter
      }

-- -------------------------
-- DELETE REFERNCE EVENT ---
-- -------------------------
instance ToBSON DeleteReferenceEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteReferenceEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "referenceUuid" BSON.=: serializeUUID (event ^. referenceUuid)
    ]

instance FromBSON DeleteReferenceEvent where
  fromBSON doc = do
    refUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    refKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    refChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    refQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc >>= \referenceUuidS -> fromString referenceUuidS
    return
      DeleteReferenceEvent
      { _deleteReferenceEventUuid = refUuid
      , _deleteReferenceEventKmUuid = refKmUuid
      , _deleteReferenceEventChapterUuid = refChapterUuid
      , _deleteReferenceEventQuestionUuid = refQuestionUuid
      , _deleteReferenceEventReferenceUuid = refReferenceUuid
      }
