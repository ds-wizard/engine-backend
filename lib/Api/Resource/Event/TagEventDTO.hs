module Api.Resource.Event.TagEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO
import Api.Resource.KnowledgeModel.PathDTO

data AddTagEventDTO = AddTagEventDTO
  { _addTagEventDTOUuid :: U.UUID
  , _addTagEventDTOPath :: PathDTO
  , _addTagEventDTOTagUuid :: U.UUID
  , _addTagEventDTOName :: String
  , _addTagEventDTODescription :: Maybe String
  , _addTagEventDTOColor :: String
  } deriving (Show, Eq, Generic)

data EditTagEventDTO = EditTagEventDTO
  { _editTagEventDTOUuid :: U.UUID
  , _editTagEventDTOPath :: PathDTO
  , _editTagEventDTOTagUuid :: U.UUID
  , _editTagEventDTOName :: EventFieldDTO String
  , _editTagEventDTODescription :: EventFieldDTO (Maybe String)
  , _editTagEventDTOColor :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteTagEventDTO = DeleteTagEventDTO
  { _deleteTagEventDTOUuid :: U.UUID
  , _deleteTagEventDTOPath :: PathDTO
  , _deleteTagEventDTOTagUuid :: U.UUID
  } deriving (Show, Eq, Generic)
