module Shared.Model.Event.Tag.TagEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField

data AddTagEvent =
  AddTagEvent
    { _addTagEventUuid :: U.UUID
    , _addTagEventParentUuid :: U.UUID
    , _addTagEventEntityUuid :: U.UUID
    , _addTagEventName :: String
    , _addTagEventDescription :: Maybe String
    , _addTagEventColor :: String
    , _addTagEventAnnotations :: [MapEntry String String]
    , _addTagEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data EditTagEvent =
  EditTagEvent
    { _editTagEventUuid :: U.UUID
    , _editTagEventParentUuid :: U.UUID
    , _editTagEventEntityUuid :: U.UUID
    , _editTagEventName :: EventField String
    , _editTagEventDescription :: EventField (Maybe String)
    , _editTagEventColor :: EventField String
    , _editTagEventAnnotations :: EventField [MapEntry String String]
    , _editTagEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data DeleteTagEvent =
  DeleteTagEvent
    { _deleteTagEventUuid :: U.UUID
    , _deleteTagEventParentUuid :: U.UUID
    , _deleteTagEventEntityUuid :: U.UUID
    , _deleteTagEventCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
