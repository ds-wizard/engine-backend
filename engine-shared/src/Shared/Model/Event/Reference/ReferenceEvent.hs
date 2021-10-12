module Shared.Model.Event.Reference.ReferenceEvent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField

data AddReferenceEvent
  = AddResourcePageReferenceEvent' AddResourcePageReferenceEvent
  | AddURLReferenceEvent' AddURLReferenceEvent
  | AddCrossReferenceEvent' AddCrossReferenceEvent
  deriving (Show, Eq, Generic)

data AddResourcePageReferenceEvent =
  AddResourcePageReferenceEvent
    { _addResourcePageReferenceEventUuid :: U.UUID
    , _addResourcePageReferenceEventParentUuid :: U.UUID
    , _addResourcePageReferenceEventEntityUuid :: U.UUID
    , _addResourcePageReferenceEventShortUuid :: String
    , _addResourcePageReferenceEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data AddURLReferenceEvent =
  AddURLReferenceEvent
    { _addURLReferenceEventUuid :: U.UUID
    , _addURLReferenceEventParentUuid :: U.UUID
    , _addURLReferenceEventEntityUuid :: U.UUID
    , _addURLReferenceEventUrl :: String
    , _addURLReferenceEventLabel :: String
    , _addURLReferenceEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data AddCrossReferenceEvent =
  AddCrossReferenceEvent
    { _addCrossReferenceEventUuid :: U.UUID
    , _addCrossReferenceEventParentUuid :: U.UUID
    , _addCrossReferenceEventEntityUuid :: U.UUID
    , _addCrossReferenceEventTargetUuid :: U.UUID
    , _addCrossReferenceEventDescription :: String
    , _addCrossReferenceEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data EditReferenceEvent
  = EditResourcePageReferenceEvent' EditResourcePageReferenceEvent
  | EditURLReferenceEvent' EditURLReferenceEvent
  | EditCrossReferenceEvent' EditCrossReferenceEvent
  deriving (Show, Eq, Generic)

data EditResourcePageReferenceEvent =
  EditResourcePageReferenceEvent
    { _editResourcePageReferenceEventUuid :: U.UUID
    , _editResourcePageReferenceEventParentUuid :: U.UUID
    , _editResourcePageReferenceEventEntityUuid :: U.UUID
    , _editResourcePageReferenceEventShortUuid :: EventField String
    , _editResourcePageReferenceEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

data EditURLReferenceEvent =
  EditURLReferenceEvent
    { _editURLReferenceEventUuid :: U.UUID
    , _editURLReferenceEventParentUuid :: U.UUID
    , _editURLReferenceEventEntityUuid :: U.UUID
    , _editURLReferenceEventUrl :: EventField String
    , _editURLReferenceEventLabel :: EventField String
    , _editURLReferenceEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

data EditCrossReferenceEvent =
  EditCrossReferenceEvent
    { _editCrossReferenceEventUuid :: U.UUID
    , _editCrossReferenceEventParentUuid :: U.UUID
    , _editCrossReferenceEventEntityUuid :: U.UUID
    , _editCrossReferenceEventTargetUuid :: EventField U.UUID
    , _editCrossReferenceEventDescription :: EventField String
    , _editCrossReferenceEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

-- --------------------------------------------
data DeleteReferenceEvent =
  DeleteReferenceEvent
    { _deleteReferenceEventUuid :: U.UUID
    , _deleteReferenceEventParentUuid :: U.UUID
    , _deleteReferenceEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
