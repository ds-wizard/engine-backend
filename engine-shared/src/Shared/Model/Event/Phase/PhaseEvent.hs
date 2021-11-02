module Shared.Model.Event.Phase.PhaseEvent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField

data AddPhaseEvent =
  AddPhaseEvent
    { _addPhaseEventUuid :: U.UUID
    , _addPhaseEventParentUuid :: U.UUID
    , _addPhaseEventEntityUuid :: U.UUID
    , _addPhaseEventTitle :: String
    , _addPhaseEventDescription :: Maybe String
    , _addPhaseEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data EditPhaseEvent =
  EditPhaseEvent
    { _editPhaseEventUuid :: U.UUID
    , _editPhaseEventParentUuid :: U.UUID
    , _editPhaseEventEntityUuid :: U.UUID
    , _editPhaseEventTitle :: EventField String
    , _editPhaseEventDescription :: EventField (Maybe String)
    , _editPhaseEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

data DeletePhaseEvent =
  DeletePhaseEvent
    { _deletePhaseEventUuid :: U.UUID
    , _deletePhaseEventParentUuid :: U.UUID
    , _deletePhaseEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
