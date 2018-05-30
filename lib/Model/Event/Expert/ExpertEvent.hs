module Model.Event.Expert.ExpertEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath

data AddExpertEvent = AddExpertEvent
  { _addExpertEventUuid :: UUID
  , _addExpertEventPath :: EventPath
  , _addExpertEventExpertUuid :: UUID
  , _addExpertEventName :: String
  , _addExpertEventEmail :: String
  } deriving (Show, Eq, Generic)

data EditExpertEvent = EditExpertEvent
  { _editExpertEventUuid :: UUID
  , _editExpertEventPath :: EventPath
  , _editExpertEventExpertUuid :: UUID
  , _editExpertEventName :: EventField String
  , _editExpertEventEmail :: EventField String
  } deriving (Show, Eq, Generic)

data DeleteExpertEvent = DeleteExpertEvent
  { _deleteExpertEventUuid :: UUID
  , _deleteExpertEventPath :: EventPath
  , _deleteExpertEventExpertUuid :: UUID
  } deriving (Show, Eq, Generic)
