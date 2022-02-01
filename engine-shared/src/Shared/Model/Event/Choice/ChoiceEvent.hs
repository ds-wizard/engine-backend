module Shared.Model.Event.Choice.ChoiceEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField

data AddChoiceEvent =
  AddChoiceEvent
    { _addChoiceEventUuid :: U.UUID
    , _addChoiceEventParentUuid :: U.UUID
    , _addChoiceEventEntityUuid :: U.UUID
    , _addChoiceEventLabel :: String
    , _addChoiceEventAnnotations :: [MapEntry String String]
    , _addChoiceEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data EditChoiceEvent =
  EditChoiceEvent
    { _editChoiceEventUuid :: U.UUID
    , _editChoiceEventParentUuid :: U.UUID
    , _editChoiceEventEntityUuid :: U.UUID
    , _editChoiceEventLabel :: EventField String
    , _editChoiceEventAnnotations :: EventField [MapEntry String String]
    , _editChoiceEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data DeleteChoiceEvent =
  DeleteChoiceEvent
    { _deleteChoiceEventUuid :: U.UUID
    , _deleteChoiceEventParentUuid :: U.UUID
    , _deleteChoiceEventEntityUuid :: U.UUID
    , _deleteChoiceEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
