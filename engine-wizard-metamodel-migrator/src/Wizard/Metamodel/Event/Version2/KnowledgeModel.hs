module Wizard.Metamodel.Event.Version2.KnowledgeModel where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version2.Common


data AddKnowledgeModelEventDTO =
  AddKnowledgeModelEventDTO
    { _addKnowledgeModelEventDTOUuid :: U.UUID
    , _addKnowledgeModelEventDTOPath :: EventPathDTO
    , _addKnowledgeModelEventDTOKmUuid :: U.UUID
    , _addKnowledgeModelEventDTOName :: String
    }
  deriving (Show, Eq, Generic)

data EditKnowledgeModelEventDTO =
  EditKnowledgeModelEventDTO
    { _editKnowledgeModelEventDTOUuid :: U.UUID
    , _editKnowledgeModelEventDTOPath :: EventPathDTO
    , _editKnowledgeModelEventDTOKmUuid :: U.UUID
    , _editKnowledgeModelEventDTOName :: EventFieldDTO String
    , _editKnowledgeModelEventDTOChapterUuids :: EventFieldDTO [U.UUID]
    , _editKnowledgeModelEventDTOTagUuids :: EventFieldDTO [U.UUID]
    , _editKnowledgeModelEventDTOIntegrationUuids :: EventFieldDTO [U.UUID]
    }
  deriving (Show, Eq, Generic)

instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON (Object o) = do
    _addKnowledgeModelEventDTOUuid <- o .: "uuid"
    _addKnowledgeModelEventDTOPath <- o .: "path"
    _addKnowledgeModelEventDTOKmUuid <- o .: "kmUuid"
    _addKnowledgeModelEventDTOName <- o .: "name"
    return AddKnowledgeModelEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON AddKnowledgeModelEventDTO {..} =
    object
      [ "eventType" .= "AddKnowledgeModelEvent"
      , "uuid" .= _addKnowledgeModelEventDTOUuid
      , "path" .= _addKnowledgeModelEventDTOPath
      , "kmUuid" .= _addKnowledgeModelEventDTOKmUuid
      , "name" .= _addKnowledgeModelEventDTOName
      ]

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON (Object o) = do
    _editKnowledgeModelEventDTOUuid <- o .: "uuid"
    _editKnowledgeModelEventDTOPath <- o .: "path"
    _editKnowledgeModelEventDTOKmUuid <- o .: "kmUuid"
    _editKnowledgeModelEventDTOName <- o .: "name"
    _editKnowledgeModelEventDTOChapterUuids <- o .: "chapterUuids"
    _editKnowledgeModelEventDTOTagUuids <- o .: "tagUuids"
    _editKnowledgeModelEventDTOIntegrationUuids <- o .: "integrationUuids"
    return EditKnowledgeModelEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditKnowledgeModelEventDTO where
  toJSON EditKnowledgeModelEventDTO {..} =
    object
      [ "eventType" .= "EditKnowledgeModelEvent"
      , "uuid" .= _editKnowledgeModelEventDTOUuid
      , "path" .= _editKnowledgeModelEventDTOPath
      , "kmUuid" .= _editKnowledgeModelEventDTOKmUuid
      , "name" .= _editKnowledgeModelEventDTOName
      , "chapterUuids" .= _editKnowledgeModelEventDTOChapterUuids
      , "tagUuids" .= _editKnowledgeModelEventDTOTagUuids
      , "integrationUuids" .= _editKnowledgeModelEventDTOIntegrationUuids
      ]

