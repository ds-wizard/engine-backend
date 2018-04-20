module Model.Event.Expert.ExpertEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField

data AddExpertEvent = AddExpertEvent
  { _addExpertEventUuid :: UUID
  , _addExpertEventKmUuid :: UUID
  , _addExpertEventChapterUuid :: UUID
  , _addExpertEventQuestionUuid :: UUID
  , _addExpertEventExpertUuid :: UUID
  , _addExpertEventName :: String
  , _addExpertEventEmail :: String
  } deriving (Show, Eq, Generic)

data EditExpertEvent = EditExpertEvent
  { _editExpertEventUuid :: UUID
  , _editExpertEventKmUuid :: UUID
  , _editExpertEventChapterUuid :: UUID
  , _editExpertEventQuestionUuid :: UUID
  , _editExpertEventExpertUuid :: UUID
  , _editExpertEventName :: EventField String
  , _editExpertEventEmail :: EventField String
  } deriving (Show, Eq, Generic)

data DeleteExpertEvent = DeleteExpertEvent
  { _deleteExpertEventUuid :: UUID
  , _deleteExpertEventKmUuid :: UUID
  , _deleteExpertEventChapterUuid :: UUID
  , _deleteExpertEventQuestionUuid :: UUID
  , _deleteExpertEventExpertUuid :: UUID
  } deriving (Show, Eq, Generic)
