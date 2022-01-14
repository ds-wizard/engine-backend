module Wizard.Metamodel.Event.Version0002.Answer where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Metamodel.Event.Version0002.Common

data AddAnswerEventDTO =
  AddAnswerEventDTO
    { _addAnswerEventDTOUuid :: U.UUID
    , _addAnswerEventDTOPath :: EventPathDTO
    , _addAnswerEventDTOAnswerUuid :: U.UUID
    , _addAnswerEventDTOLabel :: String
    , _addAnswerEventDTOAdvice :: Maybe String
    , _addAnswerEventDTOMetricMeasures :: [MetricMeasureDTO]
    }
  deriving (Show, Eq, Generic)

data EditAnswerEventDTO =
  EditAnswerEventDTO
    { _editAnswerEventDTOUuid :: U.UUID
    , _editAnswerEventDTOPath :: EventPathDTO
    , _editAnswerEventDTOAnswerUuid :: U.UUID
    , _editAnswerEventDTOLabel :: EventFieldDTO String
    , _editAnswerEventDTOAdvice :: EventFieldDTO (Maybe String)
    , _editAnswerEventDTOFollowUpUuids :: EventFieldDTO [U.UUID]
    , _editAnswerEventDTOMetricMeasures :: EventFieldDTO [MetricMeasureDTO]
    }
  deriving (Show, Eq, Generic)

data DeleteAnswerEventDTO =
  DeleteAnswerEventDTO
    { _deleteAnswerEventDTOUuid :: U.UUID
    , _deleteAnswerEventDTOPath :: EventPathDTO
    , _deleteAnswerEventDTOAnswerUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

instance FromJSON AddAnswerEventDTO where
  parseJSON (Object o) = do
    _addAnswerEventDTOUuid <- o .: "uuid"
    _addAnswerEventDTOPath <- o .: "path"
    _addAnswerEventDTOAnswerUuid <- o .: "answerUuid"
    _addAnswerEventDTOLabel <- o .: "label"
    _addAnswerEventDTOAdvice <- o .: "advice"
    _addAnswerEventDTOMetricMeasures <- o .: "metricMeasures"
    return AddAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON AddAnswerEventDTO where
  toJSON AddAnswerEventDTO {..} =
    object
      [ "eventType" .= "AddAnswerEvent"
      , "uuid" .= _addAnswerEventDTOUuid
      , "path" .= _addAnswerEventDTOPath
      , "answerUuid" .= _addAnswerEventDTOAnswerUuid
      , "label" .= _addAnswerEventDTOLabel
      , "advice" .= _addAnswerEventDTOAdvice
      , "metricMeasures" .= _addAnswerEventDTOMetricMeasures
      ]

instance FromJSON EditAnswerEventDTO where
  parseJSON (Object o) = do
    _editAnswerEventDTOUuid <- o .: "uuid"
    _editAnswerEventDTOPath <- o .: "path"
    _editAnswerEventDTOAnswerUuid <- o .: "answerUuid"
    _editAnswerEventDTOLabel <- o .: "label"
    _editAnswerEventDTOAdvice <- o .: "advice"
    _editAnswerEventDTOFollowUpUuids <- o .: "followUpUuids"
    _editAnswerEventDTOMetricMeasures <- o .: "metricMeasures"
    return EditAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON EditAnswerEventDTO where
  toJSON EditAnswerEventDTO {..} =
    object
      [ "eventType" .= "EditAnswerEvent"
      , "uuid" .= _editAnswerEventDTOUuid
      , "path" .= _editAnswerEventDTOPath
      , "answerUuid" .= _editAnswerEventDTOAnswerUuid
      , "label" .= _editAnswerEventDTOLabel
      , "advice" .= _editAnswerEventDTOAdvice
      , "followUpUuids" .= _editAnswerEventDTOFollowUpUuids
      , "metricMeasures" .= _editAnswerEventDTOMetricMeasures
      ]

instance FromJSON DeleteAnswerEventDTO where
  parseJSON (Object o) = do
    _deleteAnswerEventDTOUuid <- o .: "uuid"
    _deleteAnswerEventDTOPath <- o .: "path"
    _deleteAnswerEventDTOAnswerUuid <- o .: "answerUuid"
    return DeleteAnswerEventDTO {..}
  parseJSON _ = mzero

instance ToJSON DeleteAnswerEventDTO where
  toJSON DeleteAnswerEventDTO {..} =
    object
      [ "eventType" .= "DeleteAnswerEvent"
      , "uuid" .= _deleteAnswerEventDTOUuid
      , "path" .= _deleteAnswerEventDTOPath
      , "answerUuid" .= _deleteAnswerEventDTOAnswerUuid
      ]
