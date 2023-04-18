module WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data MoveQuestionEvent = MoveQuestionEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data MoveAnswerEvent = MoveAnswerEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data MoveChoiceEvent = MoveChoiceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data MoveExpertEvent = MoveExpertEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data MoveReferenceEvent = MoveReferenceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , targetUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
