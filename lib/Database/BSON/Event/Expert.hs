module Database.BSON.Event.Expert where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Event.Expert.AddExpertEvent
import Model.Event.Expert.DeleteExpertEvent
import Model.Event.Expert.EditExpertEvent

-- -------------------------
-- ADD EXPERT EVENT---------
-- -------------------------
instance ToBSON AddExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddExpertEvent"
    , "uuid" BSON.=: serializeUUID (event ^. aexpUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. aexpKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. aexpChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. aexpQuestionUuid)
    , "expertUuid" BSON.=: serializeUUID (event ^. aexpExpertUuid)
    , "name" BSON.=: (event ^. aexpName)
    , "email" BSON.=: (event ^. aexpEmail)
    ]

instance FromBSON AddExpertEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    expertUuid <- BSON.lookup "expertUuid" doc >>= \expertUuidS -> fromString expertUuidS
    name <- BSON.lookup "name" doc
    email <- BSON.lookup "email" doc
    return
      AddExpertEvent
      { _aexpUuid = uuid
      , _aexpKmUuid = kmUuid
      , _aexpChapterUuid = chapterUuid
      , _aexpQuestionUuid = questionUuid
      , _aexpExpertUuid = expertUuid
      , _aexpName = name
      , _aexpEmail = email
      }

-- -------------------------
-- EDIT EXPERT EVENT--------
-- -------------------------
instance ToBSON EditExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditExpertEvent"
    , "uuid" BSON.=: serializeUUID (event ^. eexpUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. eexpKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. eexpChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. eexpQuestionUuid)
    , "expertUuid" BSON.=: serializeUUID (event ^. eexpExpertUuid)
    , "name" BSON.=: (event ^. eexpName)
    , "email" BSON.=: (event ^. eexpEmail)
    ]

instance FromBSON EditExpertEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    expertUuid <- BSON.lookup "expertUuid" doc >>= \expertUuidS -> fromString expertUuidS
    name <- BSON.lookup "name" doc
    email <- BSON.lookup "email" doc
    return
      EditExpertEvent
      { _eexpUuid = uuid
      , _eexpKmUuid = kmUuid
      , _eexpChapterUuid = chapterUuid
      , _eexpQuestionUuid = questionUuid
      , _eexpExpertUuid = expertUuid
      , _eexpName = name
      , _eexpEmail = email
      }

-- -------------------------
-- DELETE EXPERT EVENT------
-- -------------------------
instance ToBSON DeleteExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteExpertEvent"
    , "uuid" BSON.=: serializeUUID (event ^. dexpUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. dexpKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. dexpChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. dexpQuestionUuid)
    , "expertUuid" BSON.=: serializeUUID (event ^. dexpExpertUuid)
    ]

instance FromBSON DeleteExpertEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    expertUuid <- BSON.lookup "expertUuid" doc >>= \expertUuidS -> fromString expertUuidS
    return
      DeleteExpertEvent
      { _dexpUuid = uuid
      , _dexpKmUuid = kmUuid
      , _dexpChapterUuid = chapterUuid
      , _dexpQuestionUuid = questionUuid
      , _dexpExpertUuid = expertUuid
      }
