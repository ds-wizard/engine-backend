module Wizard.Metamodel.Event.Version10.Chapter where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version10.Common

-- Shared.Model.Event.Chapter.ChapterEvent
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

-- Shared.Api.Resource.Event.ChapterEventJM
instance FromJSON AddChapterEvent where
  parseJSON = simpleParseJSON "_addChapterEvent"

instance ToJSON AddChapterEvent where
  toJSON = simpleToJSON' "_addChapterEvent" "eventType"

instance FromJSON EditChapterEvent where
  parseJSON = simpleParseJSON "_editChapterEvent"

instance ToJSON EditChapterEvent where
  toJSON = simpleToJSON' "_editChapterEvent" "eventType"

instance FromJSON DeleteChapterEvent where
  parseJSON = simpleParseJSON "_deleteChapterEvent"

instance ToJSON DeleteChapterEvent where
  toJSON = simpleToJSON' "_deleteChapterEvent" "eventType"
