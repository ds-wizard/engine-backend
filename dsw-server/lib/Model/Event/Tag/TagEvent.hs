module Model.Event.Tag.TagEvent where

import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField

data AddTagEvent = AddTagEvent
  { _addTagEventUuid :: U.UUID
  , _addTagEventParentUuid :: U.UUID
  , _addTagEventEntityUuid :: U.UUID
  , _addTagEventName :: String
  , _addTagEventDescription :: Maybe String
  , _addTagEventColor :: String
  } deriving (Show, Eq, Generic)

data EditTagEvent = EditTagEvent
  { _editTagEventUuid :: U.UUID
  , _editTagEventParentUuid :: U.UUID
  , _editTagEventEntityUuid :: U.UUID
  , _editTagEventName :: EventField String
  , _editTagEventDescription :: EventField (Maybe String)
  , _editTagEventColor :: EventField String
  } deriving (Show, Eq, Generic)

data DeleteTagEvent = DeleteTagEvent
  { _deleteTagEventUuid :: U.UUID
  , _deleteTagEventParentUuid :: U.UUID
  , _deleteTagEventEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
