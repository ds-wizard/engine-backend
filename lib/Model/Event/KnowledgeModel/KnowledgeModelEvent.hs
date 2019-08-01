module Model.Event.KnowledgeModel.KnowledgeModelEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.KnowledgeModel.Path

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { _addKnowledgeModelEventUuid :: UUID
  , _addKnowledgeModelEventPath :: Path
  , _addKnowledgeModelEventKmUuid :: UUID
  , _addKnowledgeModelEventName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEvent = EditKnowledgeModelEvent
  { _editKnowledgeModelEventUuid :: UUID
  , _editKnowledgeModelEventPath :: Path
  , _editKnowledgeModelEventKmUuid :: UUID
  , _editKnowledgeModelEventName :: EventField String
  , _editKnowledgeModelEventChapterUuids :: EventField [UUID]
  , _editKnowledgeModelEventTagUuids :: EventField [UUID]
  , _editKnowledgeModelEventIntegrationUuids :: EventField [UUID]
  } deriving (Show, Eq, Generic)
