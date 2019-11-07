module Api.Resource.Event.ExpertEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO

data AddExpertEventDTO = AddExpertEventDTO
  { _addExpertEventDTOUuid :: U.UUID
  , _addExpertEventDTOParentUuid :: U.UUID
  , _addExpertEventDTOEntityUuid :: U.UUID
  , _addExpertEventDTOName :: String
  , _addExpertEventDTOEmail :: String
  } deriving (Show, Eq, Generic)

data EditExpertEventDTO = EditExpertEventDTO
  { _editExpertEventDTOUuid :: U.UUID
  , _editExpertEventDTOParentUuid :: U.UUID
  , _editExpertEventDTOEntityUuid :: U.UUID
  , _editExpertEventDTOName :: EventFieldDTO String
  , _editExpertEventDTOEmail :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data DeleteExpertEventDTO = DeleteExpertEventDTO
  { _deleteExpertEventDTOUuid :: U.UUID
  , _deleteExpertEventDTOParentUuid :: U.UUID
  , _deleteExpertEventDTOEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
