module Wizard.Metamodel.Event.Version0002.Tag where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0002.Common

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

instance FromJSON AddTagEventDTO where
  parseJSON (Object o) = do
    _addTagEventDTOUuid <- o .: "uuid"
    _addTagEventDTOPath <- o .: "path"
    _addTagEventDTOTagUuid <- o .: "tagUuid"
    _addTagEventDTOName <- o .: "name"
    _addTagEventDTODescription <- o .: "description"
    _addTagEventDTOColor <- o .: "color"
    return AddTagEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddTagEventDTO where
  toJSON AddTagEventDTO {..} =
    object
      [ "eventType" .= "AddTagEvent"
      , "uuid" .= _addTagEventDTOUuid
      , "path" .= _addTagEventDTOPath
      , "tagUuid" .= _addTagEventDTOTagUuid
      , "name" .= _addTagEventDTOName
      , "description" .= _addTagEventDTODescription
      , "color" .= _addTagEventDTOColor
      ]

-- --------------------------------------------
instance FromJSON EditTagEventDTO where
  parseJSON (Object o) = do
    _editTagEventDTOUuid <- o .: "uuid"
    _editTagEventDTOPath <- o .: "path"
    _editTagEventDTOTagUuid <- o .: "tagUuid"
    _editTagEventDTOName <- o .: "name"
    _editTagEventDTODescription <- o .: "description"
    _editTagEventDTOColor <- o .: "color"
    return EditTagEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditTagEventDTO where
  toJSON EditTagEventDTO {..} =
    object
      [ "eventType" .= "EditTagEvent"
      , "uuid" .= _editTagEventDTOUuid
      , "path" .= _editTagEventDTOPath
      , "tagUuid" .= _editTagEventDTOTagUuid
      , "name" .= _editTagEventDTOName
      , "description" .= _editTagEventDTODescription
      , "color" .= _editTagEventDTOColor
      ]

-- --------------------------------------------
instance FromJSON DeleteTagEventDTO where
  parseJSON (Object o) = do
    _deleteTagEventDTOUuid <- o .: "uuid"
    _deleteTagEventDTOPath <- o .: "path"
    _deleteTagEventDTOTagUuid <- o .: "tagUuid"
    return DeleteTagEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteTagEventDTO where
  toJSON DeleteTagEventDTO {..} =
    object
      [ "eventType" .= "DeleteTagEvent"
      , "uuid" .= _deleteTagEventDTOUuid
      , "path" .= _deleteTagEventDTOPath
      , "tagUuid" .= _deleteTagEventDTOTagUuid
      ]
