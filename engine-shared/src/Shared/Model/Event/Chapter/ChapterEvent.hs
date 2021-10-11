module Shared.Model.Event.Chapter.ChapterEvent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField

data AddChapterEvent =
  AddChapterEvent
    { _addChapterEventUuid :: U.UUID
    , _addChapterEventParentUuid :: U.UUID
    , _addChapterEventEntityUuid :: U.UUID
    , _addChapterEventTitle :: String
    , _addChapterEventText :: Maybe String
    , _addChapterEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data EditChapterEvent =
  EditChapterEvent
    { _editChapterEventUuid :: U.UUID
    , _editChapterEventParentUuid :: U.UUID
    , _editChapterEventEntityUuid :: U.UUID
    , _editChapterEventTitle :: EventField String
    , _editChapterEventText :: EventField (Maybe String)
    , _editChapterEventAnnotations :: EventField (M.Map String String)
    , _editChapterEventQuestionUuids :: EventField [U.UUID]
    }
  deriving (Show, Eq, Generic)

data DeleteChapterEvent =
  DeleteChapterEvent
    { _deleteChapterEventUuid :: U.UUID
    , _deleteChapterEventParentUuid :: U.UUID
    , _deleteChapterEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
