module Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField

data AddKnowledgeModelEvent =
  AddKnowledgeModelEvent
    { _addKnowledgeModelEventUuid :: U.UUID
    , _addKnowledgeModelEventParentUuid :: U.UUID
    , _addKnowledgeModelEventEntityUuid :: U.UUID
    , _addKnowledgeModelEventAnnotations :: [MapEntry String String]
    , _addKnowledgeModelEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data EditKnowledgeModelEvent =
  EditKnowledgeModelEvent
    { _editKnowledgeModelEventUuid :: U.UUID
    , _editKnowledgeModelEventParentUuid :: U.UUID
    , _editKnowledgeModelEventEntityUuid :: U.UUID
    , _editKnowledgeModelEventAnnotations :: EventField [MapEntry String String]
    , _editKnowledgeModelEventChapterUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventTagUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventIntegrationUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventMetricUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventPhaseUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
