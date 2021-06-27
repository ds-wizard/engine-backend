module Wizard.Metamodel.Event.Version3.KnowledgeModel where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version3.Common

-- KnowledgeModelEventDTO
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

-- KnowledgeModelEventJM
instance FromJSON AddKnowledgeModelEventDTO where
  parseJSON = simpleParseJSON "_addKnowledgeModelEventDTO"

instance ToJSON AddKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "eventType" "_addKnowledgeModelEventDTO"

instance FromJSON EditKnowledgeModelEventDTO where
  parseJSON = simpleParseJSON "_editKnowledgeModelEventDTO"

instance ToJSON EditKnowledgeModelEventDTO where
  toJSON = simpleToJSON' "eventType" "_editKnowledgeModelEventDTO"
