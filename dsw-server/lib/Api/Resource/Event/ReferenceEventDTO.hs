module Api.Resource.Event.ReferenceEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO

data AddReferenceEventDTO
  = AddResourcePageReferenceEventDTO' AddResourcePageReferenceEventDTO
  | AddURLReferenceEventDTO' AddURLReferenceEventDTO
  | AddCrossReferenceEventDTO' AddCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

data AddResourcePageReferenceEventDTO = AddResourcePageReferenceEventDTO
  { _addResourcePageReferenceEventDTOUuid :: U.UUID
  , _addResourcePageReferenceEventDTOParentUuid :: U.UUID
  , _addResourcePageReferenceEventDTOEntityUuid :: U.UUID
  , _addResourcePageReferenceEventDTOShortUuid :: String
  } deriving (Show, Eq, Generic)

data AddURLReferenceEventDTO = AddURLReferenceEventDTO
  { _addURLReferenceEventDTOUuid :: U.UUID
  , _addURLReferenceEventDTOParentUuid :: U.UUID
  , _addURLReferenceEventDTOEntityUuid :: U.UUID
  , _addURLReferenceEventDTOUrl :: String
  , _addURLReferenceEventDTOLabel :: String
  } deriving (Show, Eq, Generic)

data AddCrossReferenceEventDTO = AddCrossReferenceEventDTO
  { _addCrossReferenceEventDTOUuid :: U.UUID
  , _addCrossReferenceEventDTOParentUuid :: U.UUID
  , _addCrossReferenceEventDTOEntityUuid :: U.UUID
  , _addCrossReferenceEventDTOTargetUuid :: U.UUID
  , _addCrossReferenceEventDTODescription :: String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditReferenceEventDTO
  = EditResourcePageReferenceEventDTO' EditResourcePageReferenceEventDTO
  | EditURLReferenceEventDTO' EditURLReferenceEventDTO
  | EditCrossReferenceEventDTO' EditCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

data EditResourcePageReferenceEventDTO = EditResourcePageReferenceEventDTO
  { _editResourcePageReferenceEventDTOUuid :: U.UUID
  , _editResourcePageReferenceEventDTOParentUuid :: U.UUID
  , _editResourcePageReferenceEventDTOEntityUuid :: U.UUID
  , _editResourcePageReferenceEventDTOShortUuid :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data EditURLReferenceEventDTO = EditURLReferenceEventDTO
  { _editURLReferenceEventDTOUuid :: U.UUID
  , _editURLReferenceEventDTOParentUuid :: U.UUID
  , _editURLReferenceEventDTOEntityUuid :: U.UUID
  , _editURLReferenceEventDTOUrl :: EventFieldDTO String
  , _editURLReferenceEventDTOLabel :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data EditCrossReferenceEventDTO = EditCrossReferenceEventDTO
  { _editCrossReferenceEventDTOUuid :: U.UUID
  , _editCrossReferenceEventDTOParentUuid :: U.UUID
  , _editCrossReferenceEventDTOEntityUuid :: U.UUID
  , _editCrossReferenceEventDTOTargetUuid :: EventFieldDTO U.UUID
  , _editCrossReferenceEventDTODescription :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteReferenceEventDTO = DeleteReferenceEventDTO
  { _deleteReferenceEventDTOUuid :: U.UUID
  , _deleteReferenceEventDTOParentUuid :: U.UUID
  , _deleteReferenceEventDTOEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
