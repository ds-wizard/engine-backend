module Wizard.Metamodel.Event.Version1.Chapter where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version1.Common


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

instance FromJSON AddChapterEventDTO where
  parseJSON (Object o) = do
    _addChapterEventDTOUuid <- o .: "uuid"
    _addChapterEventDTOPath <- o .: "path"
    _addChapterEventDTOChapterUuid <- o .: "chapterUuid"
    _addChapterEventDTOTitle <- o .: "title"
    _addChapterEventDTOText <- o .: "text"
    return AddChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddChapterEventDTO where
  toJSON AddChapterEventDTO {..} =
    object
      [ "eventType" .= "AddChapterEvent"
      , "uuid" .= _addChapterEventDTOUuid
      , "path" .= _addChapterEventDTOPath
      , "chapterUuid" .= _addChapterEventDTOChapterUuid
      , "title" .= _addChapterEventDTOTitle
      , "text" .= _addChapterEventDTOText
      ]

instance FromJSON EditChapterEventDTO where
  parseJSON (Object o) = do
    _editChapterEventDTOUuid <- o .: "uuid"
    _editChapterEventDTOPath <- o .: "path"
    _editChapterEventDTOChapterUuid <- o .: "chapterUuid"
    _editChapterEventDTOTitle <- o .: "title"
    _editChapterEventDTOText <- o .: "text"
    _editChapterEventDTOQuestionUuids <- o .: "questionUuids"
    return EditChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditChapterEventDTO where
  toJSON EditChapterEventDTO {..} =
    object
      [ "eventType" .= "EditChapterEvent"
      , "uuid" .= _editChapterEventDTOUuid
      , "path" .= _editChapterEventDTOPath
      , "chapterUuid" .= _editChapterEventDTOChapterUuid
      , "title" .= _editChapterEventDTOTitle
      , "text" .= _editChapterEventDTOText
      , "questionUuids" .= _editChapterEventDTOQuestionUuids
      ]

instance FromJSON DeleteChapterEventDTO where
  parseJSON (Object o) = do
    _deleteChapterEventDTOUuid <- o .: "uuid"
    _deleteChapterEventDTOPath <- o .: "path"
    _deleteChapterEventDTOChapterUuid <- o .: "chapterUuid"
    return DeleteChapterEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteChapterEventDTO where
  toJSON DeleteChapterEventDTO {..} =
    object
      [ "eventType" .= "DeleteChapterEvent"
      , "uuid" .= _deleteChapterEventDTOUuid
      , "path" .= _deleteChapterEventDTOPath
      , "chapterUuid" .= _deleteChapterEventDTOChapterUuid
      ]
