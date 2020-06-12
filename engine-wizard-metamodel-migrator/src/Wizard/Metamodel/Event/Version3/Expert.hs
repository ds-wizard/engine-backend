module Wizard.Metamodel.Event.Version3.Expert where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version3.Common


-- ExpertEventDTO
data AddExpertEventDTO =
  AddExpertEventDTO
    { _addExpertEventDTOUuid :: U.UUID
    , _addExpertEventDTOPath :: EventPathDTO
    , _addExpertEventDTOExpertUuid :: U.UUID
    , _addExpertEventDTOName :: String
    , _addExpertEventDTOEmail :: String
    }
  deriving (Show, Eq, Generic)

data EditExpertEventDTO =
  EditExpertEventDTO
    { _editExpertEventDTOUuid :: U.UUID
    , _editExpertEventDTOPath :: EventPathDTO
    , _editExpertEventDTOExpertUuid :: U.UUID
    , _editExpertEventDTOName :: EventFieldDTO String
    , _editExpertEventDTOEmail :: EventFieldDTO String
    }
  deriving (Show, Eq, Generic)

data DeleteExpertEventDTO =
  DeleteExpertEventDTO
    { _deleteExpertEventDTOUuid :: U.UUID
    , _deleteExpertEventDTOPath :: EventPathDTO
    , _deleteExpertEventDTOExpertUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- ExpertEventJM
instance FromJSON AddExpertEventDTO where
  parseJSON = simpleParseJSON "_addExpertEventDTO"

instance ToJSON AddExpertEventDTO where
  toJSON = simpleToJSON' "eventType" "_addExpertEventDTO"

instance FromJSON EditExpertEventDTO where
  parseJSON = simpleParseJSON "_editExpertEventDTO"

instance ToJSON EditExpertEventDTO where
  toJSON = simpleToJSON' "eventType" "_editExpertEventDTO"

instance FromJSON DeleteExpertEventDTO where
  parseJSON = simpleParseJSON "_deleteExpertEventDTO"

instance ToJSON DeleteExpertEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteExpertEventDTO"
