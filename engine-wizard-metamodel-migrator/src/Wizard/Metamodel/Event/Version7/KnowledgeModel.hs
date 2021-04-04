module Wizard.Metamodel.Event.Version7.KnowledgeModel where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version7.Common

-- Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
data AddKnowledgeModelEvent =
  AddKnowledgeModelEvent
    { _addKnowledgeModelEventUuid :: U.UUID
    , _addKnowledgeModelEventParentUuid :: U.UUID
    , _addKnowledgeModelEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data EditKnowledgeModelEvent =
  EditKnowledgeModelEvent
    { _editKnowledgeModelEventUuid :: U.UUID
    , _editKnowledgeModelEventParentUuid :: U.UUID
    , _editKnowledgeModelEventEntityUuid :: U.UUID
    , _editKnowledgeModelEventChapterUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventTagUuids :: EventField [U.UUID]
    , _editKnowledgeModelEventIntegrationUuids :: EventField [U.UUID]
    }
  deriving (Show, Eq, Generic)

-- Shared.Api.Resource.Event.KnowledgeModelEventJM
instance FromJSON AddKnowledgeModelEvent where
  parseJSON = simpleParseJSON "_addKnowledgeModelEvent"

instance ToJSON AddKnowledgeModelEvent where
  toJSON = simpleToJSON' "_addKnowledgeModelEvent" "eventType"

instance FromJSON EditKnowledgeModelEvent where
  parseJSON = simpleParseJSON "_editKnowledgeModelEvent"

instance ToJSON EditKnowledgeModelEvent where
  toJSON = simpleToJSON' "_editKnowledgeModelEvent" "eventType"
