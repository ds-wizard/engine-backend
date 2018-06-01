module Model.Event.Reference.ReferenceEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath

data AddReferenceEvent = AddReferenceEvent
  { _addReferenceEventUuid :: UUID
  , _addReferenceEventPath :: EventPath
  , _addReferenceEventReferenceUuid :: UUID
  , _addReferenceEventChapter :: String
  } deriving (Show, Eq, Generic)

data EditReferenceEvent = EditReferenceEvent
  { _editReferenceEventUuid :: UUID
  , _editReferenceEventPath :: EventPath
  , _editReferenceEventReferenceUuid :: UUID
  , _editReferenceEventChapter :: EventField String
  } deriving (Show, Eq, Generic)

data DeleteReferenceEvent = DeleteReferenceEvent
  { _deleteReferenceEventUuid :: UUID
  , _deleteReferenceEventPath :: EventPath
  , _deleteReferenceEventReferenceUuid :: UUID
  } deriving (Show, Eq, Generic)
