module Wizard.Metamodel.Event.Version2.Integration where

import Control.Monad
import Data.Aeson
import Data.Map
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version2.Common

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

instance FromJSON AddIntegrationEventDTO where
  parseJSON (Object o) = do
    _addIntegrationEventDTOUuid <- o .: "uuid"
    _addIntegrationEventDTOPath <- o .: "path"
    _addIntegrationEventDTOIntegrationUuid <- o .: "integrationUuid"
    _addIntegrationEventDTOIId <- o .: "id"
    _addIntegrationEventDTOName <- o .: "name"
    _addIntegrationEventDTOProps <- o .: "props"
    _addIntegrationEventDTOLogo <- o .: "logo"
    _addIntegrationEventDTORequestMethod <- o .: "requestMethod"
    _addIntegrationEventDTORequestUrl <- o .: "requestUrl"
    _addIntegrationEventDTORequestHeaders <- o .: "requestHeaders"
    _addIntegrationEventDTORequestBody <- o .: "requestBody"
    _addIntegrationEventDTOResponseListField <- o .: "responseListField"
    _addIntegrationEventDTOResponseIdField <- o .: "responseIdField"
    _addIntegrationEventDTOResponseNameField <- o .: "responseNameField"
    _addIntegrationEventDTOItemUrl <- o .: "itemUrl"
    return AddIntegrationEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddIntegrationEventDTO where
  toJSON AddIntegrationEventDTO {..} =
    object
      [ "eventType" .= "AddIntegrationEvent"
      , "uuid" .= _addIntegrationEventDTOUuid
      , "path" .= _addIntegrationEventDTOPath
      , "integrationUuid" .= _addIntegrationEventDTOIntegrationUuid
      , "id" .= _addIntegrationEventDTOIId
      , "name" .= _addIntegrationEventDTOName
      , "props" .= _addIntegrationEventDTOProps
      , "logo" .= _addIntegrationEventDTOLogo
      , "requestMethod" .= _addIntegrationEventDTORequestMethod
      , "requestUrl" .= _addIntegrationEventDTORequestUrl
      , "requestHeaders" .= _addIntegrationEventDTORequestHeaders
      , "requestBody" .= _addIntegrationEventDTORequestBody
      , "responseListField" .= _addIntegrationEventDTOResponseListField
      , "responseIdField" .= _addIntegrationEventDTOResponseIdField
      , "responseNameField" .= _addIntegrationEventDTOResponseNameField
      , "itemUrl" .= _addIntegrationEventDTOItemUrl
      ]

-- --------------------------------------------
instance FromJSON EditIntegrationEventDTO where
  parseJSON (Object o) = do
    _editIntegrationEventDTOUuid <- o .: "uuid"
    _editIntegrationEventDTOPath <- o .: "path"
    _editIntegrationEventDTOIntegrationUuid <- o .: "integrationUuid"
    _editIntegrationEventDTOIId <- o .: "id"
    _editIntegrationEventDTOName <- o .: "name"
    _editIntegrationEventDTOProps <- o .: "props"
    _editIntegrationEventDTOLogo <- o .: "logo"
    _editIntegrationEventDTORequestMethod <- o .: "requestMethod"
    _editIntegrationEventDTORequestUrl <- o .: "requestUrl"
    _editIntegrationEventDTORequestHeaders <- o .: "requestHeaders"
    _editIntegrationEventDTORequestBody <- o .: "requestBody"
    _editIntegrationEventDTOResponseListField <- o .: "responseListField"
    _editIntegrationEventDTOResponseIdField <- o .: "responseIdField"
    _editIntegrationEventDTOResponseNameField <- o .: "responseNameField"
    _editIntegrationEventDTOItemUrl <- o .: "itemUrl"
    return EditIntegrationEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditIntegrationEventDTO where
  toJSON EditIntegrationEventDTO {..} =
    object
      [ "eventType" .= "EditIntegrationEvent"
      , "uuid" .= _editIntegrationEventDTOUuid
      , "path" .= _editIntegrationEventDTOPath
      , "integrationUuid" .= _editIntegrationEventDTOIntegrationUuid
      , "id" .= _editIntegrationEventDTOIId
      , "name" .= _editIntegrationEventDTOName
      , "props" .= _editIntegrationEventDTOProps
      , "logo" .= _editIntegrationEventDTOLogo
      , "requestMethod" .= _editIntegrationEventDTORequestMethod
      , "requestUrl" .= _editIntegrationEventDTORequestUrl
      , "requestHeaders" .= _editIntegrationEventDTORequestHeaders
      , "requestBody" .= _editIntegrationEventDTORequestBody
      , "responseListField" .= _editIntegrationEventDTOResponseListField
      , "responseIdField" .= _editIntegrationEventDTOResponseIdField
      , "responseNameField" .= _editIntegrationEventDTOResponseNameField
      , "itemUrl" .= _editIntegrationEventDTOItemUrl
      ]

-- --------------------------------------------
instance FromJSON DeleteIntegrationEventDTO where
  parseJSON (Object o) = do
    _deleteIntegrationEventDTOUuid <- o .: "uuid"
    _deleteIntegrationEventDTOPath <- o .: "path"
    _deleteIntegrationEventDTOIntegrationUuid <- o .: "integrationUuid"
    return DeleteIntegrationEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteIntegrationEventDTO where
  toJSON DeleteIntegrationEventDTO {..} =
    object
      [ "eventType" .= "DeleteIntegrationEvent"
      , "uuid" .= _deleteIntegrationEventDTOUuid
      , "path" .= _deleteIntegrationEventDTOPath
      , "integrationUuid" .= _deleteIntegrationEventDTOIntegrationUuid
      ]
