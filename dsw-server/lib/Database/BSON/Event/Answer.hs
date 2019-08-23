module Database.BSON.Event.Answer where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Model.Event.Answer.AnswerEvent

-- -------------------------
-- ADD ANSWER EVENT --------
-- -------------------------
instance ToBSON AddAnswerEvent where
  toBSON AddAnswerEvent {..} =
    [ "eventType" BSON.=: "AddAnswerEvent"
    , "uuid" BSON.=: _addAnswerEventUuid
    , "parentUuid" BSON.=: _addAnswerEventParentUuid
    , "entityUuid" BSON.=: _addAnswerEventEntityUuid
    , "label" BSON.=: _addAnswerEventLabel
    , "advice" BSON.=: _addAnswerEventAdvice
    , "metricMeasures" BSON.=: _addAnswerEventMetricMeasures
    ]

instance FromBSON AddAnswerEvent where
  fromBSON doc = do
    _addAnswerEventUuid <- BSON.lookup "uuid" doc
    _addAnswerEventParentUuid <- BSON.lookup "parentUuid" doc
    _addAnswerEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addAnswerEventLabel <- BSON.lookup "label" doc
    _addAnswerEventAdvice <- BSON.lookup "advice" doc
    _addAnswerEventMetricMeasures <- BSON.lookup "metricMeasures" doc
    return AddAnswerEvent {..}

-- -------------------------
-- EDIT ANSWER EVENT -------
-- -------------------------
instance ToBSON EditAnswerEvent where
  toBSON EditAnswerEvent {..} =
    [ "eventType" BSON.=: "EditAnswerEvent"
    , "uuid" BSON.=: _editAnswerEventUuid
    , "parentUuid" BSON.=: _editAnswerEventParentUuid
    , "entityUuid" BSON.=: _editAnswerEventEntityUuid
    , "label" BSON.=: _editAnswerEventLabel
    , "advice" BSON.=: _editAnswerEventAdvice
    , "followUpUuids" BSON.=: _editAnswerEventFollowUpUuids
    , "metricMeasures" BSON.=: _editAnswerEventMetricMeasures
    ]

instance FromBSON EditAnswerEvent where
  fromBSON doc = do
    _editAnswerEventUuid <- BSON.lookup "uuid" doc
    _editAnswerEventParentUuid <- BSON.lookup "parentUuid" doc
    _editAnswerEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editAnswerEventLabel <- BSON.lookup "label" doc
    _editAnswerEventAdvice <- BSON.lookup "advice" doc
    _editAnswerEventFollowUpUuids <- BSON.lookup "followUpUuids" doc
    _editAnswerEventMetricMeasures <- BSON.lookup "metricMeasures" doc
    return EditAnswerEvent {..}

-- -------------------------
-- DELETE ANSWER EVENT -----
-- -------------------------
instance ToBSON DeleteAnswerEvent where
  toBSON DeleteAnswerEvent {..} =
    [ "eventType" BSON.=: "DeleteAnswerEvent"
    , "uuid" BSON.=: _deleteAnswerEventUuid
    , "parentUuid" BSON.=: _deleteAnswerEventParentUuid
    , "entityUuid" BSON.=: _deleteAnswerEventEntityUuid
    ]

instance FromBSON DeleteAnswerEvent where
  fromBSON doc = do
    _deleteAnswerEventUuid <- BSON.lookup "uuid" doc
    _deleteAnswerEventParentUuid <- BSON.lookup "parentUuid" doc
    _deleteAnswerEventEntityUuid <- BSON.lookup "entityUuid" doc
    return DeleteAnswerEvent {..}
