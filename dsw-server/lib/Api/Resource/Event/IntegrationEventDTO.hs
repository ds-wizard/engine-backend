module Api.Resource.Event.IntegrationEventDTO where

import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO

data AddIntegrationEventDTO = AddIntegrationEventDTO
  { _addIntegrationEventDTOUuid :: U.UUID
  , _addIntegrationEventDTOParentUuid :: U.UUID
  , _addIntegrationEventDTOEntityUuid :: U.UUID
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
  } deriving (Show, Eq, Generic)

data EditIntegrationEventDTO = EditIntegrationEventDTO
  { _editIntegrationEventDTOUuid :: U.UUID
  , _editIntegrationEventDTOParentUuid :: U.UUID
  , _editIntegrationEventDTOEntityUuid :: U.UUID
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
  } deriving (Show, Eq, Generic)

data DeleteIntegrationEventDTO = DeleteIntegrationEventDTO
  { _deleteIntegrationEventDTOUuid :: U.UUID
  , _deleteIntegrationEventDTOParentUuid :: U.UUID
  , _deleteIntegrationEventDTOEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
