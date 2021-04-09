module Wizard.Metamodel.Event.Version7.Choice where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version7.Common

-- Shared.Model.Event.Choice.ChoiceEvent
data AddChoiceEvent =
  AddChoiceEvent
    { _addChoiceEventUuid :: U.UUID
    , _addChoiceEventParentUuid :: U.UUID
    , _addChoiceEventEntityUuid :: U.UUID
    , _addChoiceEventLabel :: String
    }
  deriving (Show, Eq, Generic)

data EditChoiceEvent =
  EditChoiceEvent
    { _editChoiceEventUuid :: U.UUID
    , _editChoiceEventParentUuid :: U.UUID
    , _editChoiceEventEntityUuid :: U.UUID
    , _editChoiceEventLabel :: EventField String
    }
  deriving (Show, Eq, Generic)

data DeleteChoiceEvent =
  DeleteChoiceEvent
    { _deleteChoiceEventUuid :: U.UUID
    , _deleteChoiceEventParentUuid :: U.UUID
    , _deleteChoiceEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- Shared.Api.Resource.Event.ChoiceEventJM
instance FromJSON AddChoiceEvent where
  parseJSON = simpleParseJSON "_addChoiceEvent"

instance ToJSON AddChoiceEvent where
  toJSON = simpleToJSON' "_addChoiceEvent" "eventType"

instance FromJSON EditChoiceEvent where
  parseJSON = simpleParseJSON "_editChoiceEvent"

instance ToJSON EditChoiceEvent where
  toJSON = simpleToJSON' "_editChoiceEvent" "eventType"

instance FromJSON DeleteChoiceEvent where
  parseJSON = simpleParseJSON "_deleteChoiceEvent"

instance ToJSON DeleteChoiceEvent where
  toJSON = simpleToJSON' "_deleteChoiceEvent" "eventType"
