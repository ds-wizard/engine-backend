module Api.Resource.Event.TagEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO

data AddTagEventDTO = AddTagEventDTO
  { _addTagEventDTOUuid :: U.UUID
  , _addTagEventDTOParentUuid :: U.UUID
  , _addTagEventDTOEntityUuid :: U.UUID
  , _addTagEventDTOName :: String
  , _addTagEventDTODescription :: Maybe String
  , _addTagEventDTOColor :: String
  } deriving (Show, Eq, Generic)

data EditTagEventDTO = EditTagEventDTO
  { _editTagEventDTOUuid :: U.UUID
  , _editTagEventDTOParentUuid :: U.UUID
  , _editTagEventDTOEntityUuid :: U.UUID
  , _editTagEventDTOName :: EventFieldDTO String
  , _editTagEventDTODescription :: EventFieldDTO (Maybe String)
  , _editTagEventDTOColor :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteTagEventDTO = DeleteTagEventDTO
  { _deleteTagEventDTOUuid :: U.UUID
  , _deleteTagEventDTOParentUuid :: U.UUID
  , _deleteTagEventDTOEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
