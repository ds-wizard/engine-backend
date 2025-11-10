module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Move.MoveEvent where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Util.Hashable ()

data MoveQuestionEvent = MoveQuestionEvent
  { targetUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveQuestionEvent

data MoveAnswerEvent = MoveAnswerEvent
  { targetUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveAnswerEvent

data MoveChoiceEvent = MoveChoiceEvent
  { targetUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveChoiceEvent

data MoveExpertEvent = MoveExpertEvent
  { targetUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveExpertEvent

data MoveReferenceEvent = MoveReferenceEvent
  { targetUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveReferenceEvent
