module Wizard.Metamodel.Event.Version5.Chapter where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version5.Common

-- ChapterEventDTO
data AddChapterEventDTO =
  AddChapterEventDTO
    { _addChapterEventDTOUuid :: U.UUID
    , _addChapterEventDTOParentUuid :: U.UUID
    , _addChapterEventDTOEntityUuid :: U.UUID
    , _addChapterEventDTOTitle :: String
    , _addChapterEventDTOText :: Maybe String
    }
  deriving (Show, Eq, Generic)

data EditChapterEventDTO =
  EditChapterEventDTO
    { _editChapterEventDTOUuid :: U.UUID
    , _editChapterEventDTOParentUuid :: U.UUID
    , _editChapterEventDTOEntityUuid :: U.UUID
    , _editChapterEventDTOTitle :: EventFieldDTO String
    , _editChapterEventDTOText :: EventFieldDTO (Maybe String)
    , _editChapterEventDTOQuestionUuids :: EventFieldDTO [U.UUID]
    }
  deriving (Show, Eq, Generic)

data DeleteChapterEventDTO =
  DeleteChapterEventDTO
    { _deleteChapterEventDTOUuid :: U.UUID
    , _deleteChapterEventDTOParentUuid :: U.UUID
    , _deleteChapterEventDTOEntityUuid :: U.UUID
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
