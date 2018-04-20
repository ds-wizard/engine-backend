module Model.Event.Reference.ReferenceEvent where

import Data.UUID
import GHC.Generics

data AddReferenceEvent = AddReferenceEvent
  { _addReferenceEventUuid :: UUID
  , _addReferenceEventKmUuid :: UUID
  , _addReferenceEventChapterUuid :: UUID
  , _addReferenceEventQuestionUuid :: UUID
  , _addReferenceEventReferenceUuid :: UUID
  , _addReferenceEventChapter :: String
  } deriving (Show, Eq, Generic)

data EditReferenceEvent = EditReferenceEvent
  { _editReferenceEventUuid :: UUID
  , _editReferenceEventKmUuid :: UUID
  , _editReferenceEventChapterUuid :: UUID
  , _editReferenceEventQuestionUuid :: UUID
  , _editReferenceEventReferenceUuid :: UUID
  , _editReferenceEventChapter :: Maybe String
  } deriving (Show, Eq, Generic)

data DeleteReferenceEvent = DeleteReferenceEvent
  { _deleteReferenceEventUuid :: UUID
  , _deleteReferenceEventKmUuid :: UUID
  , _deleteReferenceEventChapterUuid :: UUID
  , _deleteReferenceEventQuestionUuid :: UUID
  , _deleteReferenceEventReferenceUuid :: UUID
  } deriving (Show, Eq, Generic)
