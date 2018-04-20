module Model.Event.Chapter.ChapterEvent where

import Data.UUID
import GHC.Generics

data AddChapterEvent = AddChapterEvent
  { _addChapterEventUuid :: UUID
  , _addChapterEventKmUuid :: UUID
  , _addChapterEventChapterUuid :: UUID
  , _addChapterEventTitle :: String
  , _addChapterEventText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEvent = EditChapterEvent
  { _editChapterEventUuid :: UUID
  , _editChapterEventKmUuid :: UUID
  , _editChapterEventChapterUuid :: UUID
  , _editChapterEventTitle :: Maybe String
  , _editChapterEventText :: Maybe String
  , _editChapterEventQuestionIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEvent = DeleteChapterEvent
  { _deleteChapterEventUuid :: UUID
  , _deleteChapterEventKmUuid :: UUID
  , _deleteChapterEventChapterUuid :: UUID
  } deriving (Show, Eq, Generic)