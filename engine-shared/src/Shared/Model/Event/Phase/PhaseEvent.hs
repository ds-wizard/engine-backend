module Shared.Model.Event.Phase.PhaseEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField

data AddPhaseEvent =
  AddPhaseEvent
    { _addPhaseEventUuid :: U.UUID
    , _addPhaseEventParentUuid :: U.UUID
    , _addPhaseEventEntityUuid :: U.UUID
    , _addPhaseEventTitle :: String
    , _addPhaseEventDescription :: Maybe String
    , _addPhaseEventAnnotations :: [MapEntry String String]
    , _addPhaseEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data EditPhaseEvent =
  EditPhaseEvent
    { _editPhaseEventUuid :: U.UUID
    , _editPhaseEventParentUuid :: U.UUID
    , _editPhaseEventEntityUuid :: U.UUID
    , _editPhaseEventTitle :: EventField String
    , _editPhaseEventDescription :: EventField (Maybe String)
    , _editPhaseEventAnnotations :: EventField [MapEntry String String]
    , _editPhaseEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data DeletePhaseEvent =
  DeletePhaseEvent
    { _deletePhaseEventUuid :: U.UUID
    , _deletePhaseEventParentUuid :: U.UUID
    , _deletePhaseEventEntityUuid :: U.UUID
    , _deletePhaseEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
