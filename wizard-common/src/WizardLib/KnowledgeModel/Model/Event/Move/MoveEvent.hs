module WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Common.Util.Hashable ()

data MoveQuestionEvent = MoveQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveQuestionEvent

data MoveAnswerEvent = MoveAnswerEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveAnswerEvent

data MoveChoiceEvent = MoveChoiceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveChoiceEvent

data MoveExpertEvent = MoveExpertEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveExpertEvent

data MoveReferenceEvent = MoveReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable MoveReferenceEvent
