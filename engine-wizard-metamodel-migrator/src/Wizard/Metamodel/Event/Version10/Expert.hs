module Wizard.Metamodel.Event.Version10.Expert where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version10.Common

-- Shared.Model.Event.Expert.ExpertEvent
data AddExpertEvent =
  AddExpertEvent
    { _addExpertEventUuid :: U.UUID
    , _addExpertEventParentUuid :: U.UUID
    , _addExpertEventEntityUuid :: U.UUID
    , _addExpertEventName :: String
    , _addExpertEventEmail :: String
    , _addExpertEventAnnotations :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data EditExpertEvent =
  EditExpertEvent
    { _editExpertEventUuid :: U.UUID
    , _editExpertEventParentUuid :: U.UUID
    , _editExpertEventEntityUuid :: U.UUID
    , _editExpertEventName :: EventField String
    , _editExpertEventEmail :: EventField String
    , _editExpertEventAnnotations :: EventField (M.Map String String)
    }
  deriving (Show, Eq, Generic)

data DeleteExpertEvent =
  DeleteExpertEvent
    { _deleteExpertEventUuid :: U.UUID
    , _deleteExpertEventParentUuid :: U.UUID
    , _deleteExpertEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- Shared.Model.Event.Expert.ExpertEvent
instance FromJSON AddExpertEvent where
  parseJSON = simpleParseJSON "_addExpertEvent"

instance ToJSON AddExpertEvent where
  toJSON = simpleToJSON' "_addExpertEvent" "eventType"

instance FromJSON EditExpertEvent where
  parseJSON = simpleParseJSON "_editExpertEvent"

instance ToJSON EditExpertEvent where
  toJSON = simpleToJSON' "_editExpertEvent" "eventType"

instance FromJSON DeleteExpertEvent where
  parseJSON = simpleParseJSON "_deleteExpertEvent"

instance ToJSON DeleteExpertEvent where
  toJSON = simpleToJSON' "_deleteExpertEvent" "eventType"
