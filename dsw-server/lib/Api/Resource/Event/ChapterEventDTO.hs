module Api.Resource.Event.ChapterEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO

data AddChapterEventDTO = AddChapterEventDTO
  { _addChapterEventDTOUuid :: U.UUID
  , _addChapterEventDTOParentUuid :: U.UUID
  , _addChapterEventDTOEntityUuid :: U.UUID
  , _addChapterEventDTOTitle :: String
  , _addChapterEventDTOText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEventDTO = EditChapterEventDTO
  { _editChapterEventDTOUuid :: U.UUID
  , _editChapterEventDTOParentUuid :: U.UUID
  , _editChapterEventDTOEntityUuid :: U.UUID
  , _editChapterEventDTOTitle :: EventFieldDTO String
  , _editChapterEventDTOText :: EventFieldDTO String
  , _editChapterEventDTOQuestionUuids :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEventDTO = DeleteChapterEventDTO
  { _deleteChapterEventDTOUuid :: U.UUID
  , _deleteChapterEventDTOParentUuid :: U.UUID
  , _deleteChapterEventDTOEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
