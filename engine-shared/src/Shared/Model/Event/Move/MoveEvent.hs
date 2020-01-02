module Shared.Model.Event.Move.MoveEvent where

import qualified Data.UUID as U
import GHC.Generics

data MoveQuestionEvent =
  MoveQuestionEvent
    { _moveQuestionEventUuid :: U.UUID
    , _moveQuestionEventParentUuid :: U.UUID
    , _moveQuestionEventEntityUuid :: U.UUID
    , _moveQuestionEventTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveAnswerEvent =
  MoveAnswerEvent
    { _moveAnswerEventUuid :: U.UUID
    , _moveAnswerEventParentUuid :: U.UUID
    , _moveAnswerEventEntityUuid :: U.UUID
    , _moveAnswerEventTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveExpertEvent =
  MoveExpertEvent
    { _moveExpertEventUuid :: U.UUID
    , _moveExpertEventParentUuid :: U.UUID
    , _moveExpertEventEntityUuid :: U.UUID
    , _moveExpertEventTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveReferenceEvent =
  MoveReferenceEvent
    { _moveReferenceEventUuid :: U.UUID
    , _moveReferenceEventParentUuid :: U.UUID
    , _moveReferenceEventEntityUuid :: U.UUID
    , _moveReferenceEventTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
