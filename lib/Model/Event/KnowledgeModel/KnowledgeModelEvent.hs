module Model.Event.KnowledgeModel.KnowledgeModelEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { _addKnowledgeModelEventUuid :: UUID
  , _addKnowledgeModelEventPath :: EventPath
  , _addKnowledgeModelEventKmUuid :: UUID
  , _addKnowledgeModelEventName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEvent = EditKnowledgeModelEvent
  { _editKnowledgeModelEventUuid :: UUID
  , _editKnowledgeModelEventPath :: EventPath
  , _editKnowledgeModelEventKmUuid :: UUID
  , _editKnowledgeModelEventName :: EventField String
  , _editKnowledgeModelEventChapterUuids :: EventField [UUID]
  , _editKnowledgeModelEventTagUuids :: EventField [UUID]
  , _editKnowledgeModelEventIntegrationUuids :: EventField [UUID]
  } deriving (Show, Eq, Generic)
