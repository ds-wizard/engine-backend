module Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Util.Hashable ()

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddKnowledgeModelEvent

data EditKnowledgeModelEvent = EditKnowledgeModelEvent
  { annotations :: EventField [MapEntry String String]
  , chapterUuids :: EventField [U.UUID]
  , tagUuids :: EventField [U.UUID]
  , integrationUuids :: EventField [U.UUID]
  , metricUuids :: EventField [U.UUID]
  , phaseUuids :: EventField [U.UUID]
  , resourceCollectionUuids :: EventField [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditKnowledgeModelEvent
