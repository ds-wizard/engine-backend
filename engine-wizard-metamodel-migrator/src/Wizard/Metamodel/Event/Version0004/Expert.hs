module Wizard.Metamodel.Event.Version0004.Expert where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0004.Common

-- ExpertEventDTO
data AddExpertEventDTO =
  AddExpertEventDTO
    { _addExpertEventDTOUuid :: U.UUID
    , _addExpertEventDTOParentUuid :: U.UUID
    , _addExpertEventDTOEntityUuid :: U.UUID
    , _addExpertEventDTOName :: String
    , _addExpertEventDTOEmail :: String
    }
  deriving (Show, Eq, Generic)

data EditExpertEventDTO =
  EditExpertEventDTO
    { _editExpertEventDTOUuid :: U.UUID
    , _editExpertEventDTOParentUuid :: U.UUID
    , _editExpertEventDTOEntityUuid :: U.UUID
    , _editExpertEventDTOName :: EventFieldDTO String
    , _editExpertEventDTOEmail :: EventFieldDTO String
    }
  deriving (Show, Eq, Generic)

data DeleteExpertEventDTO =
  DeleteExpertEventDTO
    { _deleteExpertEventDTOUuid :: U.UUID
    , _deleteExpertEventDTOParentUuid :: U.UUID
    , _deleteExpertEventDTOEntityUuid :: U.UUID
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
