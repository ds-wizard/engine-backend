module Database.BSON.Event.Expert where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Database.BSON.Event.EventField
import LensesConfig
import Model.Event.Expert.ExpertEvent

-- -------------------------
-- ADD EXPERT EVENT---------
-- -------------------------
instance ToBSON AddExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddExpertEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "expertUuid" BSON.=: serializeUUID (event ^. expertUuid)
    , "name" BSON.=: (event ^. name)
    , "email" BSON.=: (event ^. email)
    ]

instance FromBSON AddExpertEvent where
  fromBSON doc = do
    expUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    expKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    expChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    expQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    expExpertUuid <- BSON.lookup "expertUuid" doc >>= \expertUuidS -> fromString expertUuidS
    expName <- BSON.lookup "name" doc
    expEmail <- BSON.lookup "email" doc
    return
      AddExpertEvent
      { _addExpertEventUuid = expUuid
      , _addExpertEventKmUuid = expKmUuid
      , _addExpertEventChapterUuid = expChapterUuid
      , _addExpertEventQuestionUuid = expQuestionUuid
      , _addExpertEventExpertUuid = expExpertUuid
      , _addExpertEventName = expName
      , _addExpertEventEmail = expEmail
      }

-- -------------------------
-- EDIT EXPERT EVENT--------
-- -------------------------
instance ToBSON EditExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditExpertEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "expertUuid" BSON.=: serializeUUID (event ^. expertUuid)
    , "name" BSON.=: (event ^. name)
    , "email" BSON.=: (event ^. email)
    ]

instance FromBSON EditExpertEvent where
  fromBSON doc = do
    expUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    expKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    expChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    expQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    expExpertUuid <- BSON.lookup "expertUuid" doc >>= \expertUuidS -> fromString expertUuidS
    expName <- BSON.lookup "name" doc
    expEmail <- BSON.lookup "email" doc
    return
      EditExpertEvent
      { _editExpertEventUuid = expUuid
      , _editExpertEventKmUuid = expKmUuid
      , _editExpertEventChapterUuid = expChapterUuid
      , _editExpertEventQuestionUuid = expQuestionUuid
      , _editExpertEventExpertUuid = expExpertUuid
      , _editExpertEventName = expName
      , _editExpertEventEmail = expEmail
      }

-- -------------------------
-- DELETE EXPERT EVENT------
-- -------------------------
instance ToBSON DeleteExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteExpertEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "expertUuid" BSON.=: serializeUUID (event ^. expertUuid)
    ]

instance FromBSON DeleteExpertEvent where
  fromBSON doc = do
    expUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    expKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    expChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    expQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    expExpertUuid <- BSON.lookup "expertUuid" doc >>= \expertUuidS -> fromString expertUuidS
    return
      DeleteExpertEvent
      { _deleteExpertEventUuid = expUuid
      , _deleteExpertEventKmUuid = expKmUuid
      , _deleteExpertEventChapterUuid = expChapterUuid
      , _deleteExpertEventQuestionUuid = expQuestionUuid
      , _deleteExpertEventExpertUuid = expExpertUuid
      }
