module Api.Resource.Event.AnswerEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO

data AddAnswerEventDTO = AddAnswerEventDTO
  { _addAnswerEventDTOUuid :: U.UUID
  , _addAnswerEventDTOParentUuid :: U.UUID
  , _addAnswerEventDTOEntityUuid :: U.UUID
  , _addAnswerEventDTOLabel :: String
  , _addAnswerEventDTOAdvice :: Maybe String
  , _addAnswerEventDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq, Generic)

data EditAnswerEventDTO = EditAnswerEventDTO
  { _editAnswerEventDTOUuid :: U.UUID
  , _editAnswerEventDTOParentUuid :: U.UUID
  , _editAnswerEventDTOEntityUuid :: U.UUID
  , _editAnswerEventDTOLabel :: EventFieldDTO String
  , _editAnswerEventDTOAdvice :: EventFieldDTO (Maybe String)
  , _editAnswerEventDTOFollowUpUuids :: EventFieldDTO [U.UUID]
  , _editAnswerEventDTOMetricMeasures :: EventFieldDTO [MetricMeasureDTO]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEventDTO = DeleteAnswerEventDTO
  { _deleteAnswerEventDTOUuid :: U.UUID
  , _deleteAnswerEventDTOParentUuid :: U.UUID
  , _deleteAnswerEventDTOEntityUuid :: U.UUID
  } deriving (Show, Eq, Generic)
