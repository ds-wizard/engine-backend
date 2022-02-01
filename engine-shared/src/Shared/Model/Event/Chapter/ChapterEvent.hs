module Shared.Model.Event.Chapter.ChapterEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField

data AddChapterEvent =
  AddChapterEvent
    { _addChapterEventUuid :: U.UUID
    , _addChapterEventParentUuid :: U.UUID
    , _addChapterEventEntityUuid :: U.UUID
    , _addChapterEventTitle :: String
    , _addChapterEventText :: Maybe String
    , _addChapterEventAnnotations :: [MapEntry String String]
    , _addChapterEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data EditChapterEvent =
  EditChapterEvent
    { _editChapterEventUuid :: U.UUID
    , _editChapterEventParentUuid :: U.UUID
    , _editChapterEventEntityUuid :: U.UUID
    , _editChapterEventTitle :: EventField String
    , _editChapterEventText :: EventField (Maybe String)
    , _editChapterEventAnnotations :: EventField [MapEntry String String]
    , _editChapterEventQuestionUuids :: EventField [U.UUID]
    , _editChapterEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data DeleteChapterEvent =
  DeleteChapterEvent
    { _deleteChapterEventUuid :: U.UUID
    , _deleteChapterEventParentUuid :: U.UUID
    , _deleteChapterEventEntityUuid :: U.UUID
    , _deleteChapterEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
