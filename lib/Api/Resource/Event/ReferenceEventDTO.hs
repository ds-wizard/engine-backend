module Api.Resource.Event.ReferenceEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.EventPathDTO

data AddReferenceEventDTO
  = AddResourcePageReferenceEventDTO' AddResourcePageReferenceEventDTO
  | AddURLReferenceEventDTO' AddURLReferenceEventDTO
  | AddCrossReferenceEventDTO' AddCrossReferenceEventDTO
  deriving (Show, Eq, Generic)

data AddResourcePageReferenceEventDTO = AddResourcePageReferenceEventDTO
  { _addResourcePageReferenceEventDTOUuid :: U.UUID
  , _addResourcePageReferenceEventDTOPath :: EventPathDTO
  , _addResourcePageReferenceEventDTOReferenceUuid :: U.UUID
  , _addResourcePageReferenceEventDTOShortUuid :: String
  } deriving (Show, Eq, Generic)

data AddURLReferenceEventDTO = AddURLReferenceEventDTO
  { _addURLReferenceEventDTOUuid :: U.UUID
  , _addURLReferenceEventDTOPath :: EventPathDTO
  , _addURLReferenceEventDTOReferenceUuid :: U.UUID
  , _addURLReferenceEventDTOUrl :: String
  , _addURLReferenceEventDTOLabel :: String
  } deriving (Show, Eq, Generic)

data AddCrossReferenceEventDTO = AddCrossReferenceEventDTO
  { _addCrossReferenceEventDTOUuid :: U.UUID
  , _addCrossReferenceEventDTOPath :: EventPathDTO
  , _addCrossReferenceEventDTOReferenceUuid :: U.UUID
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
  , _editResourcePageReferenceEventDTOPath :: EventPathDTO
  , _editResourcePageReferenceEventDTOReferenceUuid :: U.UUID
  , _editResourcePageReferenceEventDTOShortUuid :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data EditURLReferenceEventDTO = EditURLReferenceEventDTO
  { _editURLReferenceEventDTOUuid :: U.UUID
  , _editURLReferenceEventDTOPath :: EventPathDTO
  , _editURLReferenceEventDTOReferenceUuid :: U.UUID
  , _editURLReferenceEventDTOUrl :: EventFieldDTO String
  , _editURLReferenceEventDTOLabel :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

data EditCrossReferenceEventDTO = EditCrossReferenceEventDTO
  { _editCrossReferenceEventDTOUuid :: U.UUID
  , _editCrossReferenceEventDTOPath :: EventPathDTO
  , _editCrossReferenceEventDTOReferenceUuid :: U.UUID
  , _editCrossReferenceEventDTOTargetUuid :: EventFieldDTO U.UUID
  , _editCrossReferenceEventDTODescription :: EventFieldDTO String
  } deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteReferenceEventDTO = DeleteReferenceEventDTO
  { _deleteReferenceEventDTOUuid :: U.UUID
  , _deleteReferenceEventDTOPath :: EventPathDTO
  , _deleteReferenceEventDTOReferenceUuid :: U.UUID
  } deriving (Show, Eq, Generic)
