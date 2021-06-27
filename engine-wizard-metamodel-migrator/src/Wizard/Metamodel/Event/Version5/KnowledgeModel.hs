module Wizard.Metamodel.Event.Version5.KnowledgeModel where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version5.Common

-- KnowledgeModelEventDTO
data AddKnowledgeModelEventDTO =
  AddKnowledgeModelEventDTO
    { _addKnowledgeModelEventDTOUuid :: U.UUID
    , _addKnowledgeModelEventDTOParentUuid :: U.UUID
    , _addKnowledgeModelEventDTOEntityUuid :: U.UUID
    , _addKnowledgeModelEventDTOName :: String
    }
  deriving (Show, Eq, Generic)

data EditKnowledgeModelEventDTO =
  EditKnowledgeModelEventDTO
    { _editKnowledgeModelEventDTOUuid :: U.UUID
    , _editKnowledgeModelEventDTOParentUuid :: U.UUID
    , _editKnowledgeModelEventDTOEntityUuid :: U.UUID
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

-- MoveEventDTO
data MoveQuestionEventDTO =
  MoveQuestionEventDTO
    { _moveQuestionEventDTOUuid :: U.UUID
    , _moveQuestionEventDTOParentUuid :: U.UUID
    , _moveQuestionEventDTOEntityUuid :: U.UUID
    , _moveQuestionEventDTOTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveAnswerEventDTO =
  MoveAnswerEventDTO
    { _moveAnswerEventDTOUuid :: U.UUID
    , _moveAnswerEventDTOParentUuid :: U.UUID
    , _moveAnswerEventDTOEntityUuid :: U.UUID
    , _moveAnswerEventDTOTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveExpertEventDTO =
  MoveExpertEventDTO
    { _moveExpertEventDTOUuid :: U.UUID
    , _moveExpertEventDTOParentUuid :: U.UUID
    , _moveExpertEventDTOEntityUuid :: U.UUID
    , _moveExpertEventDTOTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

data MoveReferenceEventDTO =
  MoveReferenceEventDTO
    { _moveReferenceEventDTOUuid :: U.UUID
    , _moveReferenceEventDTOParentUuid :: U.UUID
    , _moveReferenceEventDTOEntityUuid :: U.UUID
    , _moveReferenceEventDTOTargetUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- MoveEventJM
instance FromJSON MoveQuestionEventDTO where
  parseJSON = simpleParseJSON "_moveQuestionEventDTO"

instance ToJSON MoveQuestionEventDTO where
  toJSON = simpleToJSON' "eventType" "_moveQuestionEventDTO"

instance FromJSON MoveAnswerEventDTO where
  parseJSON = simpleParseJSON "_moveAnswerEventDTO"

instance ToJSON MoveAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_moveAnswerEventDTO"

instance FromJSON MoveExpertEventDTO where
  parseJSON = simpleParseJSON "_moveExpertEventDTO"

instance ToJSON MoveExpertEventDTO where
  toJSON = simpleToJSON' "eventType" "_moveExpertEventDTO"

instance FromJSON MoveReferenceEventDTO where
  parseJSON = simpleParseJSON "_moveReferenceEventDTO"

instance ToJSON MoveReferenceEventDTO where
  toJSON = simpleToJSON' "eventType" "_moveReferenceEventDTO"
