module Model.Event.Chapter.ChapterEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.KnowledgeModel.Path

data AddChapterEvent = AddChapterEvent
  { _addChapterEventUuid :: UUID
  , _addChapterEventPath :: Path
  , _addChapterEventChapterUuid :: UUID
  , _addChapterEventTitle :: String
  , _addChapterEventText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEvent = EditChapterEvent
  { _editChapterEventUuid :: UUID
  , _editChapterEventPath :: Path
  , _editChapterEventChapterUuid :: UUID
  , _editChapterEventTitle :: EventField String
  , _editChapterEventText :: EventField String
  , _editChapterEventQuestionUuids :: EventField [UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEvent = DeleteChapterEvent
  { _deleteChapterEventUuid :: UUID
  , _deleteChapterEventPath :: Path
  , _deleteChapterEventChapterUuid :: UUID
  } deriving (Show, Eq, Generic)
