module Wizard.Metamodel.Event.Version0003.Integration where

import Data.Aeson
import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0003.Common

-- IntegrationEventDTO
data AddIntegrationEventDTO =
  AddIntegrationEventDTO
    { _addIntegrationEventDTOUuid :: U.UUID
    , _addIntegrationEventDTOPath :: EventPathDTO
    , _addIntegrationEventDTOIntegrationUuid :: U.UUID
    , _addIntegrationEventDTOIId :: String
    , _addIntegrationEventDTOName :: String
    , _addIntegrationEventDTOProps :: [String]
    , _addIntegrationEventDTOLogo :: String
    , _addIntegrationEventDTORequestMethod :: String
    , _addIntegrationEventDTORequestUrl :: String
    , _addIntegrationEventDTORequestHeaders :: Map String String
    , _addIntegrationEventDTORequestBody :: String
    , _addIntegrationEventDTOResponseListField :: String
    , _addIntegrationEventDTOResponseIdField :: String
    , _addIntegrationEventDTOResponseNameField :: String
    , _addIntegrationEventDTOItemUrl :: String
    }
  deriving (Show, Eq, Generic)

data EditIntegrationEventDTO =
  EditIntegrationEventDTO
    { _editIntegrationEventDTOUuid :: U.UUID
    , _editIntegrationEventDTOPath :: EventPathDTO
    , _editIntegrationEventDTOIntegrationUuid :: U.UUID
    , _editIntegrationEventDTOIId :: EventFieldDTO String
    , _editIntegrationEventDTOName :: EventFieldDTO String
    , _editIntegrationEventDTOProps :: EventFieldDTO [String]
    , _editIntegrationEventDTOLogo :: EventFieldDTO String
    , _editIntegrationEventDTORequestMethod :: EventFieldDTO String
    , _editIntegrationEventDTORequestUrl :: EventFieldDTO String
    , _editIntegrationEventDTORequestHeaders :: EventFieldDTO (Map String String)
    , _editIntegrationEventDTORequestBody :: EventFieldDTO String
    , _editIntegrationEventDTOResponseListField :: EventFieldDTO String
    , _editIntegrationEventDTOResponseIdField :: EventFieldDTO String
    , _editIntegrationEventDTOResponseNameField :: EventFieldDTO String
    , _editIntegrationEventDTOItemUrl :: EventFieldDTO String
    }
  deriving (Show, Eq, Generic)

data DeleteIntegrationEventDTO =
  DeleteIntegrationEventDTO
    { _deleteIntegrationEventDTOUuid :: U.UUID
    , _deleteIntegrationEventDTOPath :: EventPathDTO
    , _deleteIntegrationEventDTOIntegrationUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- IntegrationEventJM
instance FromJSON AddIntegrationEventDTO where
  parseJSON = simpleParseJSON "_addIntegrationEventDTO"

instance ToJSON AddIntegrationEventDTO where
  toJSON = simpleToJSON' "eventType" "_addIntegrationEventDTO"

-- --------------------------------------------
instance FromJSON EditIntegrationEventDTO where
  parseJSON = simpleParseJSON "_editIntegrationEventDTO"

instance ToJSON EditIntegrationEventDTO where
  toJSON = simpleToJSON' "eventType" "_editIntegrationEventDTO"

-- --------------------------------------------
instance FromJSON DeleteIntegrationEventDTO where
  parseJSON = simpleParseJSON "_deleteIntegrationEventDTO"

instance ToJSON DeleteIntegrationEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteIntegrationEventDTO"
