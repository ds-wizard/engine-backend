module Wizard.Metamodel.Event.Version0011.Phase where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0011.Common

-- Shared.Model.Event.Phase.PhaseEvent
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

-- Shared.Api.Resource.Event.PhaseEventJM
instance FromJSON AddPhaseEvent where
  parseJSON = simpleParseJSON "_addPhaseEvent"

instance ToJSON AddPhaseEvent where
  toJSON = simpleToJSON' "_addPhaseEvent" "eventType"

-- --------------------------------------------
instance FromJSON EditPhaseEvent where
  parseJSON = simpleParseJSON "_editPhaseEvent"

instance ToJSON EditPhaseEvent where
  toJSON = simpleToJSON' "_editPhaseEvent" "eventType"

-- --------------------------------------------
instance FromJSON DeletePhaseEvent where
  parseJSON = simpleParseJSON "_deletePhaseEvent"

instance ToJSON DeletePhaseEvent where
  toJSON = simpleToJSON' "_deletePhaseEvent" "eventType"
