module Wizard.Metamodel.Event.Version4.Answer where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version4.Common

-- AnswerEventDTO
data AddAnswerEventDTO =
  AddAnswerEventDTO
    { _addAnswerEventDTOUuid :: U.UUID
    , _addAnswerEventDTOParentUuid :: U.UUID
    , _addAnswerEventDTOEntityUuid :: U.UUID
    , _addAnswerEventDTOLabel :: String
    , _addAnswerEventDTOAdvice :: Maybe String
    , _addAnswerEventDTOMetricMeasures :: [MetricMeasureDTO]
    }
  deriving (Show, Eq, Generic)

data EditAnswerEventDTO =
  EditAnswerEventDTO
    { _editAnswerEventDTOUuid :: U.UUID
    , _editAnswerEventDTOParentUuid :: U.UUID
    , _editAnswerEventDTOEntityUuid :: U.UUID
    , _editAnswerEventDTOLabel :: EventFieldDTO String
    , _editAnswerEventDTOAdvice :: EventFieldDTO (Maybe String)
    , _editAnswerEventDTOFollowUpUuids :: EventFieldDTO [U.UUID]
    , _editAnswerEventDTOMetricMeasures :: EventFieldDTO [MetricMeasureDTO]
    }
  deriving (Show, Eq, Generic)

data DeleteAnswerEventDTO =
  DeleteAnswerEventDTO
    { _deleteAnswerEventDTOUuid :: U.UUID
    , _deleteAnswerEventDTOParentUuid :: U.UUID
    , _deleteAnswerEventDTOEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- AnsvertEventJM
instance FromJSON AddAnswerEventDTO where
  parseJSON = simpleParseJSON "_addAnswerEventDTO"

instance ToJSON AddAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_addAnswerEventDTO"

instance FromJSON EditAnswerEventDTO where
  parseJSON = simpleParseJSON "_editAnswerEventDTO"

instance ToJSON EditAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_editAnswerEventDTO"

instance FromJSON DeleteAnswerEventDTO where
  parseJSON = simpleParseJSON "_deleteAnswerEventDTO"

instance ToJSON DeleteAnswerEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteAnswerEventDTO"
