module Model.Event.KnowledgeModel.KnowledgeModelEvent where

import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { _addKnowledgeModelEventUuid :: U.UUID
  , _addKnowledgeModelEventParentUuid :: U.UUID
  , _addKnowledgeModelEventEntityUuid :: U.UUID
  , _addKnowledgeModelEventName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEvent = EditKnowledgeModelEvent
  { _editKnowledgeModelEventUuid :: U.UUID
  , _editKnowledgeModelEventParentUuid :: U.UUID
  , _editKnowledgeModelEventEntityUuid :: U.UUID
  , _editKnowledgeModelEventName :: EventField String
  , _editKnowledgeModelEventChapterUuids :: EventField [U.UUID]
  , _editKnowledgeModelEventTagUuids :: EventField [U.UUID]
  , _editKnowledgeModelEventIntegrationUuids :: EventField [U.UUID]
  } deriving (Show, Eq, Generic)
