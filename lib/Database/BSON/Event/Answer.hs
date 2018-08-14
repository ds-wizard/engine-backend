module Database.BSON.Event.Answer where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.Event.EventField ()
import Database.BSON.Event.EventPath ()
import LensesConfig
import Model.Event.Answer.AnswerEvent

-- -------------------------
-- ADD ANSWER EVENT --------
-- -------------------------
instance ToBSON AddAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    , "label" BSON.=: (event ^. label)
    , "advice" BSON.=: (event ^. advice)
    , "metricMeasures" BSON.=: (event ^. metricMeasures)
    ]

instance FromBSON AddAnswerEvent where
  fromBSON doc = do
    ansUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    ansPath <- BSON.lookup "path" doc
    ansAnswerUuid <- deserializeMaybeUUID $ BSON.lookup "answerUuid" doc
    ansLabel <- BSON.lookup "label" doc
    ansAdvice <- BSON.lookup "advice" doc
    ansMetricMeasures <- BSON.lookup "metricMeasures" doc
    return
      AddAnswerEvent
      { _addAnswerEventUuid = ansUuid
      , _addAnswerEventPath = ansPath
      , _addAnswerEventAnswerUuid = ansAnswerUuid
      , _addAnswerEventLabel = ansLabel
      , _addAnswerEventAdvice = ansAdvice
      , _addAnswerEventMetricMeasures = ansMetricMeasures
      }

-- -------------------------
-- EDIT ANSWER EVENT -------
-- -------------------------
instance ToBSON EditAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    , "label" BSON.=: (event ^. label)
    , "advice" BSON.=: (event ^. advice)
    , "followUpIds" BSON.=: serializeEventFieldUUIDList (event ^. followUpIds)
    , "metricMeasures" BSON.=: (event ^. metricMeasures)
    ]

instance FromBSON EditAnswerEvent where
  fromBSON doc = do
    ansUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    ansPath <- BSON.lookup "path" doc
    ansAnswerUuid <- deserializeMaybeUUID $ BSON.lookup "answerUuid" doc
    ansLabel <- BSON.lookup "label" doc
    ansAdvice <- BSON.lookup "advice" doc
    let ansFollowUpIds = deserializeEventFieldUUIDList $ BSON.lookup "followUpIds" doc
    ansMetricMeasures <- BSON.lookup "metricMeasures" doc
    return
      EditAnswerEvent
      { _editAnswerEventUuid = ansUuid
      , _editAnswerEventPath = ansPath
      , _editAnswerEventAnswerUuid = ansAnswerUuid
      , _editAnswerEventLabel = ansLabel
      , _editAnswerEventAdvice = ansAdvice
      , _editAnswerEventFollowUpIds = ansFollowUpIds
      , _editAnswerEventMetricMeasures = ansMetricMeasures
      }

-- -------------------------
-- DELETE ANSWER EVENT -----
-- -------------------------
instance ToBSON DeleteAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    ]

instance FromBSON DeleteAnswerEvent where
  fromBSON doc = do
    ansUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    ansPath <- BSON.lookup "path" doc
    ansAnswerUuid <- deserializeMaybeUUID $ BSON.lookup "answerUuid" doc
    return
      DeleteAnswerEvent
      {_deleteAnswerEventUuid = ansUuid, _deleteAnswerEventPath = ansPath, _deleteAnswerEventAnswerUuid = ansAnswerUuid}
