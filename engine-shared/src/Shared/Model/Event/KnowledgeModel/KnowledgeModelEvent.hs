module Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField

data AddKnowledgeModelEvent =
  AddKnowledgeModelEvent
    { _addKnowledgeModelEventUuid :: U.UUID
    , _addKnowledgeModelEventParentUuid :: U.UUID
    , _addKnowledgeModelEventEntityUuid :: U.UUID
    , _addKnowledgeModelEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data EditKnowledgeModelEvent =
  EditKnowledgeModelEvent
    { _editKnowledgeModelEventUuid :: U.UUID
    , _editKnowledgeModelEventParentUuid :: U.UUID
    , _editKnowledgeModelEventEntityUuid :: U.UUID
    , _editKnowledgeModelEventAnnotations :: EventField (M.Map String String)
    , _editKnowledgeModelEventChapterUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventTagUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventIntegrationUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventMetricUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventPhaseUuids :: EventField [U.UUID]
    }
  deriving (Show, Eq, Generic)
