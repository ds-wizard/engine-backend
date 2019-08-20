module Model.Event.Answer.AnswerEvent where

import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel

data AddAnswerEvent = AddAnswerEvent
  { _addAnswerEventUuid :: U.UUID
  , _addAnswerEventParentUuid :: U.UUID
  , _addAnswerEventEntityUuid :: U.UUID
  , _addAnswerEventLabel :: String
  , _addAnswerEventAdvice :: Maybe String
  , _addAnswerEventMetricMeasures :: [MetricMeasure]
  } deriving (Show, Eq, Generic)

data EditAnswerEvent = EditAnswerEvent
  { _editAnswerEventUuid :: U.UUID
  , _editAnswerEventParentUuid :: U.UUID
  , _editAnswerEventEntityUuid :: U.UUID
  , _editAnswerEventLabel :: EventField String
  , _editAnswerEventAdvice :: EventField (Maybe String)
  , _editAnswerEventFollowUpUuids :: EventField [U.UUID]
  , _editAnswerEventMetricMeasures :: EventField [MetricMeasure]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEvent = DeleteAnswerEvent
  { _deleteAnswerEventUuid :: U.UUID
  , _deleteAnswerEventParentUuid :: U.UUID
  , _deleteAnswerEventEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
