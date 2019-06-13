module Api.Resource.Event.ChapterEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO
import Api.Resource.KnowledgeModel.PathDTO

data AddChapterEventDTO = AddChapterEventDTO
  { _addChapterEventDTOUuid :: U.UUID
  , _addChapterEventDTOPath :: PathDTO
  , _addChapterEventDTOChapterUuid :: U.UUID
  , _addChapterEventDTOTitle :: String
  , _addChapterEventDTOText :: String
  } deriving (Show, Eq, Generic)

data EditChapterEventDTO = EditChapterEventDTO
  { _editChapterEventDTOUuid :: U.UUID
  , _editChapterEventDTOPath :: PathDTO
  , _editChapterEventDTOChapterUuid :: U.UUID
  , _editChapterEventDTOTitle :: EventFieldDTO String
  , _editChapterEventDTOText :: EventFieldDTO String
  , _editChapterEventDTOQuestionUuids :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)

data DeleteChapterEventDTO = DeleteChapterEventDTO
  { _deleteChapterEventDTOUuid :: U.UUID
  , _deleteChapterEventDTOPath :: PathDTO
  , _deleteChapterEventDTOChapterUuid :: U.UUID
  } deriving (Show, Eq, Generic)
