module Shared.Model.Event.Choice.ChoiceEvent where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField

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
