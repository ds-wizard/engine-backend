module Wizard.Metamodel.Event.Version0006.Answer where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0006.Common

-- Shared.Model.Event.Answer.AnswerEvent
data AddAnswerEvent =
  AddAnswerEvent
    { _addAnswerEventUuid :: U.UUID
    , _addAnswerEventParentUuid :: U.UUID
    , _addAnswerEventEntityUuid :: U.UUID
    , _addAnswerEventLabel :: String
    , _addAnswerEventAdvice :: Maybe String
    , _addAnswerEventMetricMeasures :: [MetricMeasure]
    }
  deriving (Show, Eq, Generic)

data EditAnswerEvent =
  EditAnswerEvent
    { _editAnswerEventUuid :: U.UUID
    , _editAnswerEventParentUuid :: U.UUID
    , _editAnswerEventEntityUuid :: U.UUID
    , _editAnswerEventLabel :: EventField String
    , _editAnswerEventAdvice :: EventField (Maybe String)
    , _editAnswerEventFollowUpUuids :: EventField [U.UUID]
    , _editAnswerEventMetricMeasures :: EventField [MetricMeasure]
    }
  deriving (Show, Eq, Generic)

data DeleteAnswerEvent =
  DeleteAnswerEvent
    { _deleteAnswerEventUuid :: U.UUID
    , _deleteAnswerEventParentUuid :: U.UUID
    , _deleteAnswerEventEntityUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

-- Shared.Api.Resource.Event.AnswerEventJM
instance FromJSON AddAnswerEvent where
  parseJSON = simpleParseJSON "_addAnswerEvent"

instance ToJSON AddAnswerEvent where
  toJSON = simpleToJSON' "_addAnswerEvent" "eventType"

instance FromJSON EditAnswerEvent where
  parseJSON = simpleParseJSON "_editAnswerEvent"

instance ToJSON EditAnswerEvent where
  toJSON = simpleToJSON' "_editAnswerEvent" "eventType"

instance FromJSON DeleteAnswerEvent where
  parseJSON = simpleParseJSON "_deleteAnswerEvent"

instance ToJSON DeleteAnswerEvent where
  toJSON = simpleToJSON' "_deleteAnswerEvent" "eventType"
