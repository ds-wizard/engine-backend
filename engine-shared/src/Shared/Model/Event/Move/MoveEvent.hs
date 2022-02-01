module Shared.Model.Event.Move.MoveEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data MoveQuestionEvent =
  MoveQuestionEvent
    { _moveQuestionEventUuid :: U.UUID
    , _moveQuestionEventParentUuid :: U.UUID
    , _moveQuestionEventEntityUuid :: U.UUID
    , _moveQuestionEventTargetUuid :: U.UUID
    , _moveQuestionEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MoveAnswerEvent =
  MoveAnswerEvent
    { _moveAnswerEventUuid :: U.UUID
    , _moveAnswerEventParentUuid :: U.UUID
    , _moveAnswerEventEntityUuid :: U.UUID
    , _moveAnswerEventTargetUuid :: U.UUID
    , _moveAnswerEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MoveChoiceEvent =
  MoveChoiceEvent
    { _moveChoiceEventUuid :: U.UUID
    , _moveChoiceEventParentUuid :: U.UUID
    , _moveChoiceEventEntityUuid :: U.UUID
    , _moveChoiceEventTargetUuid :: U.UUID
    , _moveChoiceEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MoveExpertEvent =
  MoveExpertEvent
    { _moveExpertEventUuid :: U.UUID
    , _moveExpertEventParentUuid :: U.UUID
    , _moveExpertEventEntityUuid :: U.UUID
    , _moveExpertEventTargetUuid :: U.UUID
    , _moveExpertEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data MoveReferenceEvent =
  MoveReferenceEvent
    { _moveReferenceEventUuid :: U.UUID
    , _moveReferenceEventParentUuid :: U.UUID
    , _moveReferenceEventEntityUuid :: U.UUID
    , _moveReferenceEventTargetUuid :: U.UUID
    , _moveReferenceEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
