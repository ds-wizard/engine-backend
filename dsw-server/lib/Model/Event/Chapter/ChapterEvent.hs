module Model.Event.Chapter.ChapterEvent where

import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField

data AddChapterEvent = AddChapterEvent
  { _addChapterEventUuid :: U.UUID
  , _addChapterEventParentUuid :: U.UUID
  , _addChapterEventEntityUuid :: U.UUID
  , _addChapterEventTitle :: String
  , _addChapterEventText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEvent = EditChapterEvent
  { _editChapterEventUuid :: U.UUID
  , _editChapterEventParentUuid :: U.UUID
  , _editChapterEventEntityUuid :: U.UUID
  , _editChapterEventTitle :: EventField String
  , _editChapterEventText :: EventField String
  , _editChapterEventQuestionUuids :: EventField [U.UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEvent = DeleteChapterEvent
  { _deleteChapterEventUuid :: U.UUID
  , _deleteChapterEventParentUuid :: U.UUID
  , _deleteChapterEventEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
