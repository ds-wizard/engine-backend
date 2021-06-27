module Wizard.Metamodel.Event.Version3.Tag where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version3.Common

-- TagEventDTO
data AddTagEventDTO =
  AddTagEventDTO
    { _addTagEventDTOUuid :: U.UUID
    , _addTagEventDTOPath :: EventPathDTO
    , _addTagEventDTOTagUuid :: U.UUID
    , _addTagEventDTOName :: String
    , _addTagEventDTODescription :: Maybe String
    , _addTagEventDTOColor :: String
    }
  deriving (Show, Eq, Generic)

data EditTagEventDTO =
  EditTagEventDTO
    { _editTagEventDTOUuid :: U.UUID
    , _editTagEventDTOPath :: EventPathDTO
    , _editTagEventDTOTagUuid :: U.UUID
    , _editTagEventDTOName :: EventFieldDTO String
    , _editTagEventDTODescription :: EventFieldDTO (Maybe String)
    , _editTagEventDTOColor :: EventFieldDTO String
    }
  deriving (Show, Eq, Generic)

data DeleteTagEventDTO =
  DeleteTagEventDTO
    { _deleteTagEventDTOUuid :: U.UUID
    , _deleteTagEventDTOPath :: EventPathDTO
    , _deleteTagEventDTOTagUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- TagEventJM
instance FromJSON AddTagEventDTO where
  parseJSON = simpleParseJSON "_addTagEventDTO"

instance ToJSON AddTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_addTagEventDTO"

-- --------------------------------------------
instance FromJSON EditTagEventDTO where
  parseJSON = simpleParseJSON "_editTagEventDTO"

instance ToJSON EditTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_editTagEventDTO"

-- --------------------------------------------
instance FromJSON DeleteTagEventDTO where
  parseJSON = simpleParseJSON "_deleteTagEventDTO"

instance ToJSON DeleteTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteTagEventDTO"
