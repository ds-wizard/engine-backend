module Wizard.Metamodel.Event.Version0007.Tag where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0007.Common

-- Shared.Model.Event.Tag.TagEvent
data AddTagEvent =
  AddTagEvent
    { _addTagEventUuid :: U.UUID
    , _addTagEventParentUuid :: U.UUID
    , _addTagEventEntityUuid :: U.UUID
    , _addTagEventName :: String
    , _addTagEventDescription :: Maybe String
    , _addTagEventColor :: String
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
    }
  deriving (Show, Eq, Generic)

data DeleteTagEvent =
  DeleteTagEvent
    { _deleteTagEventUuid :: U.UUID
    , _deleteTagEventParentUuid :: U.UUID
    , _deleteTagEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- Shared.Api.Resource.Event.TagEventJM
instance FromJSON AddTagEvent where
  parseJSON = simpleParseJSON "_addTagEvent"

instance ToJSON AddTagEvent where
  toJSON = simpleToJSON' "_addTagEvent" "eventType"

-- --------------------------------------------
instance FromJSON EditTagEvent where
  parseJSON = simpleParseJSON "_editTagEvent"

instance ToJSON EditTagEvent where
  toJSON = simpleToJSON' "_editTagEvent" "eventType"

-- --------------------------------------------
instance FromJSON DeleteTagEvent where
  parseJSON = simpleParseJSON "_deleteTagEvent"

instance ToJSON DeleteTagEvent where
  toJSON = simpleToJSON' "_deleteTagEvent" "eventType"
