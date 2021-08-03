module Wizard.Metamodel.Event.Version2.Expert where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version2.Common

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

instance FromJSON AddExpertEventDTO where
  parseJSON (Object o) = do
    _addExpertEventDTOUuid <- o .: "uuid"
    _addExpertEventDTOPath <- o .: "path"
    _addExpertEventDTOExpertUuid <- o .: "expertUuid"
    _addExpertEventDTOName <- o .: "name"
    _addExpertEventDTOEmail <- o .: "email"
    return AddExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddExpertEventDTO where
  toJSON AddExpertEventDTO {..} =
    object
      [ "eventType" .= "AddExpertEvent"
      , "uuid" .= _addExpertEventDTOUuid
      , "path" .= _addExpertEventDTOPath
      , "expertUuid" .= _addExpertEventDTOExpertUuid
      , "name" .= _addExpertEventDTOName
      , "email" .= _addExpertEventDTOEmail
      ]

instance FromJSON EditExpertEventDTO where
  parseJSON (Object o) = do
    _editExpertEventDTOUuid <- o .: "uuid"
    _editExpertEventDTOPath <- o .: "path"
    _editExpertEventDTOExpertUuid <- o .: "expertUuid"
    _editExpertEventDTOName <- o .: "name"
    _editExpertEventDTOEmail <- o .: "email"
    return EditExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditExpertEventDTO where
  toJSON EditExpertEventDTO {..} =
    object
      [ "eventType" .= "EditExpertEvent"
      , "uuid" .= _editExpertEventDTOUuid
      , "path" .= _editExpertEventDTOPath
      , "expertUuid" .= _editExpertEventDTOExpertUuid
      , "name" .= _editExpertEventDTOName
      , "email" .= _editExpertEventDTOEmail
      ]

instance FromJSON DeleteExpertEventDTO where
  parseJSON (Object o) = do
    _deleteExpertEventDTOUuid <- o .: "uuid"
    _deleteExpertEventDTOPath <- o .: "path"
    _deleteExpertEventDTOExpertUuid <- o .: "expertUuid"
    return DeleteExpertEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteExpertEventDTO where
  toJSON DeleteExpertEventDTO {..} =
    object
      [ "eventType" .= "DeleteExpertEvent"
      , "uuid" .= _deleteExpertEventDTOUuid
      , "path" .= _deleteExpertEventDTOPath
      , "expertUuid" .= _deleteExpertEventDTOExpertUuid
      ]
