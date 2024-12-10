module WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEvent where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.Common.Util.Hashable ()
import WizardLib.KnowledgeModel.Model.Event.EventField

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddKnowledgeModelEvent

data EditKnowledgeModelEvent = EditKnowledgeModelEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , annotations :: EventField [MapEntry String String]
  , chapterUuids :: EventField [U.UUID]
  , tagUuids :: EventField [U.UUID]
  , integrationUuids :: EventField [U.UUID]
  , metricUuids :: EventField [U.UUID]
  , phaseUuids :: EventField [U.UUID]
  , resourceCollectionUuids :: EventField [U.UUID]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditKnowledgeModelEvent
