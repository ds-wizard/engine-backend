module Registry.Database.BSON.Event.Move where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Registry.Database.BSON.Common ()
import Registry.Database.BSON.Event.EventField ()
import Shared.Model.Event.Move.MoveEvent

-- -------------------------
-- QUESTION MOVE EVENT -----
-- -------------------------
instance ToBSON MoveQuestionEvent where
  toBSON MoveQuestionEvent {..} =
    [ "eventType" BSON.=: "MoveQuestionEvent"
    , "uuid" BSON.=: _moveQuestionEventUuid
    , "parentUuid" BSON.=: _moveQuestionEventParentUuid
    , "entityUuid" BSON.=: _moveQuestionEventEntityUuid
    , "targetUuid" BSON.=: _moveQuestionEventTargetUuid
    ]

instance FromBSON MoveQuestionEvent where
  fromBSON doc = do
    _moveQuestionEventUuid <- BSON.lookup "uuid" doc
    _moveQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _moveQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _moveQuestionEventTargetUuid <- BSON.lookup "targetUuid" doc
    return MoveQuestionEvent {..}

-- -------------------------
-- ANSWER MOVE EVENT -------
-- -------------------------
instance ToBSON MoveAnswerEvent where
  toBSON MoveAnswerEvent {..} =
    [ "eventType" BSON.=: "MoveAnswerEvent"
    , "uuid" BSON.=: _moveAnswerEventUuid
    , "parentUuid" BSON.=: _moveAnswerEventParentUuid
    , "entityUuid" BSON.=: _moveAnswerEventEntityUuid
    , "targetUuid" BSON.=: _moveAnswerEventTargetUuid
    ]

instance FromBSON MoveAnswerEvent where
  fromBSON doc = do
    _moveAnswerEventUuid <- BSON.lookup "uuid" doc
    _moveAnswerEventParentUuid <- BSON.lookup "parentUuid" doc
    _moveAnswerEventEntityUuid <- BSON.lookup "entityUuid" doc
    _moveAnswerEventTargetUuid <- BSON.lookup "targetUuid" doc
    return MoveAnswerEvent {..}

-- -------------------------
-- EXPERT MOVE EVENT -------
-- -------------------------
instance ToBSON MoveExpertEvent where
  toBSON MoveExpertEvent {..} =
    [ "eventType" BSON.=: "MoveExpertEvent"
    , "uuid" BSON.=: _moveExpertEventUuid
    , "parentUuid" BSON.=: _moveExpertEventParentUuid
    , "entityUuid" BSON.=: _moveExpertEventEntityUuid
    , "targetUuid" BSON.=: _moveExpertEventTargetUuid
    ]

instance FromBSON MoveExpertEvent where
  fromBSON doc = do
    _moveExpertEventUuid <- BSON.lookup "uuid" doc
    _moveExpertEventParentUuid <- BSON.lookup "parentUuid" doc
    _moveExpertEventEntityUuid <- BSON.lookup "entityUuid" doc
    _moveExpertEventTargetUuid <- BSON.lookup "targetUuid" doc
    return MoveExpertEvent {..}

-- -------------------------
-- REFERENCE MOVE EVENT ----
-- -------------------------
instance ToBSON MoveReferenceEvent where
  toBSON MoveReferenceEvent {..} =
    [ "eventType" BSON.=: "MoveReferenceEvent"
    , "uuid" BSON.=: _moveReferenceEventUuid
    , "parentUuid" BSON.=: _moveReferenceEventParentUuid
    , "entityUuid" BSON.=: _moveReferenceEventEntityUuid
    , "targetUuid" BSON.=: _moveReferenceEventTargetUuid
    ]

instance FromBSON MoveReferenceEvent where
  fromBSON doc = do
    _moveReferenceEventUuid <- BSON.lookup "uuid" doc
    _moveReferenceEventParentUuid <- BSON.lookup "parentUuid" doc
    _moveReferenceEventEntityUuid <- BSON.lookup "entityUuid" doc
    _moveReferenceEventTargetUuid <- BSON.lookup "targetUuid" doc
    return MoveReferenceEvent {..}
