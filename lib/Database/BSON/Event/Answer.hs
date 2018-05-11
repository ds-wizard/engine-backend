module Database.BSON.Event.Answer where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.Event.EventField ()
import LensesConfig
import Model.Event.Answer.AnswerEvent

-- -------------------------
-- ADD ANSWER EVENT --------
-- -------------------------
instance ToBSON AddAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    , "label" BSON.=: (event ^. label)
    , "advice" BSON.=: (event ^. advice)
    ]

instance FromBSON AddAnswerEvent where
  fromBSON doc = do
    ansUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    ansKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    ansChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    ansQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    ansAnswerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    ansLabel <- BSON.lookup "label" doc
    ansAdvice <- BSON.lookup "advice" doc
    return
      AddAnswerEvent
      { _addAnswerEventUuid = ansUuid
      , _addAnswerEventKmUuid = ansKmUuid
      , _addAnswerEventChapterUuid = ansChapterUuid
      , _addAnswerEventQuestionUuid = ansQuestionUuid
      , _addAnswerEventAnswerUuid = ansAnswerUuid
      , _addAnswerEventLabel = ansLabel
      , _addAnswerEventAdvice = ansAdvice
      }

-- -------------------------
-- EDIT ANSWER EVENT -------
-- -------------------------
instance ToBSON EditAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    , "label" BSON.=: (event ^. label)
    , "advice" BSON.=: (event ^. advice)
    , "followUpIds" BSON.=: serializeEventFieldUUIDList (event ^. followUpIds)
    ]

instance FromBSON EditAnswerEvent where
  fromBSON doc = do
    ansUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    ansKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    ansChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    ansQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    ansAnswerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    ansLabel <- BSON.lookup "label" doc
    ansAdvice <- BSON.lookup "advice" doc
    let ansFollowUpIds = deserializeEventFieldUUIDList $ BSON.lookup "followUpIds" doc
    return
      EditAnswerEvent
      { _editAnswerEventUuid = ansUuid
      , _editAnswerEventKmUuid = ansKmUuid
      , _editAnswerEventChapterUuid = ansChapterUuid
      , _editAnswerEventQuestionUuid = ansQuestionUuid
      , _editAnswerEventAnswerUuid = ansAnswerUuid
      , _editAnswerEventLabel = ansLabel
      , _editAnswerEventAdvice = ansAdvice
      , _editAnswerEventFollowUpIds = ansFollowUpIds
      }

-- -------------------------
-- DELETE ANSWER EVENT -----
-- -------------------------
instance ToBSON DeleteAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    ]

instance FromBSON DeleteAnswerEvent where
  fromBSON doc = do
    ansUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    ansKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    ansChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    ansQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    ansAnswerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    return
      DeleteAnswerEvent
      { _deleteAnswerEventUuid = ansUuid
      , _deleteAnswerEventKmUuid = ansKmUuid
      , _deleteAnswerEventChapterUuid = ansChapterUuid
      , _deleteAnswerEventQuestionUuid = ansQuestionUuid
      , _deleteAnswerEventAnswerUuid = ansAnswerUuid
      }
