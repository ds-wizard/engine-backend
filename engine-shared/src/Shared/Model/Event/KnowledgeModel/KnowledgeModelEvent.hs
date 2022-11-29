module Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

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
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
