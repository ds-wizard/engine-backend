module Wizard.Api.Resource.Event.MoveEventDTO where

import qualified Data.UUID as U
import GHC.Generics

data MoveQuestionEventDTO =
  MoveQuestionEventDTO
    { _moveQuestionEventDTOUuid :: U.UUID
    , _moveQuestionEventDTOParentUuid :: U.UUID
    , _moveQuestionEventDTOEntityUuid :: U.UUID
    , _moveQuestionEventDTOTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveAnswerEventDTO =
  MoveAnswerEventDTO
    { _moveAnswerEventDTOUuid :: U.UUID
    , _moveAnswerEventDTOParentUuid :: U.UUID
    , _moveAnswerEventDTOEntityUuid :: U.UUID
    , _moveAnswerEventDTOTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveExpertEventDTO =
  MoveExpertEventDTO
    { _moveExpertEventDTOUuid :: U.UUID
    , _moveExpertEventDTOParentUuid :: U.UUID
    , _moveExpertEventDTOEntityUuid :: U.UUID
    , _moveExpertEventDTOTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveReferenceEventDTO =
  MoveReferenceEventDTO
    { _moveReferenceEventDTOUuid :: U.UUID
    , _moveReferenceEventDTOParentUuid :: U.UUID
    , _moveReferenceEventDTOEntityUuid :: U.UUID
    , _moveReferenceEventDTOTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
