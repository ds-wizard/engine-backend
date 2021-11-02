module Shared.Model.Event.Expert.ExpertEvent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.EventField

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
