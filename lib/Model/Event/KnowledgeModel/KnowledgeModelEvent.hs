module Model.Event.KnowledgeModel.KnowledgeModelEvent where

import Data.UUID
import GHC.Generics

data AddKnowledgeModelEvent = AddKnowledgeModelEvent
  { _addKnowledgeModelEventUuid :: UUID
  , _addKnowledgeModelEventKmUuid :: UUID
  , _addKnowledgeModelEventName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEvent = EditKnowledgeModelEvent
  { _editKnowledgeModelEventUuid :: UUID
  , _editKnowledgeModelEventKmUuid :: UUID
  , _editKnowledgeModelEventName :: Maybe String
  , _editKnowledgeModelEventChapterIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)
