module Wizard.Metamodel.Event.Version0010.Phase where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0010.Common

-- Shared.Model.Event.Phase.PhaseEvent
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
