module Wizard.Metamodel.Event.Version3.Chapter where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version3.Common

-- ChapterEventDTO
data AddChapterEventDTO =
  AddChapterEventDTO
    { _addChapterEventDTOUuid :: U.UUID
    , _addChapterEventDTOPath :: EventPathDTO
    , _addChapterEventDTOChapterUuid :: U.UUID
    , _addChapterEventDTOTitle :: String
    , _addChapterEventDTOText :: String
    }
  deriving (Show, Eq, Generic)

data EditChapterEventDTO =
  EditChapterEventDTO
    { _editChapterEventDTOUuid :: U.UUID
    , _editChapterEventDTOPath :: EventPathDTO
    , _editChapterEventDTOChapterUuid :: U.UUID
    , _editChapterEventDTOTitle :: EventFieldDTO String
    , _editChapterEventDTOText :: EventFieldDTO String
    , _editChapterEventDTOQuestionUuids :: EventFieldDTO [U.UUID]
    }
  deriving (Show, Eq, Generic)

data DeleteChapterEventDTO =
  DeleteChapterEventDTO
    { _deleteChapterEventDTOUuid :: U.UUID
    , _deleteChapterEventDTOPath :: EventPathDTO
    , _deleteChapterEventDTOChapterUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- ChapterEventJM
instance FromJSON AddChapterEventDTO where
  parseJSON = simpleParseJSON "_addChapterEventDTO"

instance ToJSON AddChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_addChapterEventDTO"

instance FromJSON EditChapterEventDTO where
  parseJSON = simpleParseJSON "_editChapterEventDTO"

instance ToJSON EditChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_editChapterEventDTO"

instance FromJSON DeleteChapterEventDTO where
  parseJSON = simpleParseJSON "_deleteChapterEventDTO"

instance ToJSON DeleteChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteChapterEventDTO"
