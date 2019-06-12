module Api.Resource.Event.KnowledgeModelEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.EventPathDTO

data AddKnowledgeModelEventDTO = AddKnowledgeModelEventDTO
  { _addKnowledgeModelEventDTOUuid :: U.UUID
  , _addKnowledgeModelEventDTOPath :: EventPathDTO
  , _addKnowledgeModelEventDTOKmUuid :: U.UUID
  , _addKnowledgeModelEventDTOName :: String
  } deriving (Show, Eq, Generic)

data EditKnowledgeModelEventDTO = EditKnowledgeModelEventDTO
  { _editKnowledgeModelEventDTOUuid :: U.UUID
  , _editKnowledgeModelEventDTOPath :: EventPathDTO
  , _editKnowledgeModelEventDTOKmUuid :: U.UUID
  , _editKnowledgeModelEventDTOName :: EventFieldDTO String
  , _editKnowledgeModelEventDTOChapterUuids :: EventFieldDTO [U.UUID]
  , _editKnowledgeModelEventDTOTagUuids :: EventFieldDTO [U.UUID]
  , _editKnowledgeModelEventDTOIntegrationUuids :: EventFieldDTO [U.UUID]
  } deriving (Show, Eq, Generic)
