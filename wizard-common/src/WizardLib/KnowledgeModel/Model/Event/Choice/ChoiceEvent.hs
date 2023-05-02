module WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.KnowledgeModel.Model.Event.EventField

data AddChoiceEvent = AddChoiceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , aLabel :: String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data EditChoiceEvent = EditChoiceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , aLabel :: EventField String
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DeleteChoiceEvent = DeleteChoiceEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
