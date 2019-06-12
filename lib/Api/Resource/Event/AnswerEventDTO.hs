module Api.Resource.Event.AnswerEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.EventPathDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO

data AddAnswerEventDTO = AddAnswerEventDTO
  { _addAnswerEventDTOUuid :: U.UUID
  , _addAnswerEventDTOPath :: EventPathDTO
  , _addAnswerEventDTOAnswerUuid :: U.UUID
  , _addAnswerEventDTOLabel :: String
  , _addAnswerEventDTOAdvice :: Maybe String
  , _addAnswerEventDTOMetricMeasures :: [MetricMeasureDTO]
  } deriving (Show, Eq, Generic)

data EditAnswerEventDTO = EditAnswerEventDTO
  { _editAnswerEventDTOUuid :: U.UUID
  , _editAnswerEventDTOPath :: EventPathDTO
  , _editAnswerEventDTOAnswerUuid :: U.UUID
  , _editAnswerEventDTOLabel :: EventFieldDTO String
  , _editAnswerEventDTOAdvice :: EventFieldDTO (Maybe String)
  , _editAnswerEventDTOFollowUpUuids :: EventFieldDTO [U.UUID]
  , _editAnswerEventDTOMetricMeasures :: EventFieldDTO [MetricMeasureDTO]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEventDTO = DeleteAnswerEventDTO
  { _deleteAnswerEventDTOUuid :: U.UUID
  , _deleteAnswerEventDTOPath :: EventPathDTO
  , _deleteAnswerEventDTOAnswerUuid :: U.UUID
  } deriving (Show, Eq, Generic)
