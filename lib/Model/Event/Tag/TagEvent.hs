module Model.Event.Tag.TagEvent where

import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath

data AddTagEvent = AddTagEvent
  { _addTagEventUuid :: U.UUID
  , _addTagEventPath :: EventPath
  , _addTagEventTagUuid :: U.UUID
  , _addTagEventName :: String
  , _addTagEventDescription :: Maybe String
  , _addTagEventColor :: String
  } deriving (Show, Eq, Generic)

data EditTagEvent = EditTagEvent
  { _editTagEventUuid :: U.UUID
  , _editTagEventPath :: EventPath
  , _editTagEventTagUuid :: U.UUID
  , _editTagEventName :: EventField String
  , _editTagEventDescription :: EventField (Maybe String)
  , _editTagEventColor :: EventField String
  } deriving (Show, Eq, Generic)

data DeleteTagEvent = DeleteTagEvent
  { _deleteTagEventUuid :: U.UUID
  , _deleteTagEventPath :: EventPath
  , _deleteTagEventTagUuid :: U.UUID
  } deriving (Show, Eq, Generic)
