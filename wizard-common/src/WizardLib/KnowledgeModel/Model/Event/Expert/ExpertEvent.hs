module WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.KnowledgeModel.Model.Event.EventField

data AddExpertEvent = AddExpertEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , name :: String
  , email :: String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data EditExpertEvent = EditExpertEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , name :: EventField String
  , email :: EventField String
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DeleteExpertEvent = DeleteExpertEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
