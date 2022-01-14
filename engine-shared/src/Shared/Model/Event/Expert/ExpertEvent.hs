module Shared.Model.Event.Expert.ExpertEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField

data AddExpertEvent =
  AddExpertEvent
    { _addExpertEventUuid :: U.UUID
    , _addExpertEventParentUuid :: U.UUID
    , _addExpertEventEntityUuid :: U.UUID
    , _addExpertEventName :: String
    , _addExpertEventEmail :: String
    , _addExpertEventAnnotations :: [MapEntry String String]
    , _addExpertEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data EditExpertEvent =
  EditExpertEvent
    { _editExpertEventUuid :: U.UUID
    , _editExpertEventParentUuid :: U.UUID
    , _editExpertEventEntityUuid :: U.UUID
    , _editExpertEventName :: EventField String
    , _editExpertEventEmail :: EventField String
    , _editExpertEventAnnotations :: EventField [MapEntry String String]
    , _editExpertEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data DeleteExpertEvent =
  DeleteExpertEvent
    { _deleteExpertEventUuid :: U.UUID
    , _deleteExpertEventParentUuid :: U.UUID
    , _deleteExpertEventEntityUuid :: U.UUID
    , _deleteExpertEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
