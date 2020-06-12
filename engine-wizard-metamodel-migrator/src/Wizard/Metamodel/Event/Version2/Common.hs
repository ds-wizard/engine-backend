module Wizard.Metamodel.Event.Version2.Common where

import Control.Monad
import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

-- EventField DTO
data EventFieldDTO a
  = NothingChangedDTO
  | ChangedValueDTO a
  deriving (Show, Eq)

instance Functor EventFieldDTO where
  fmap f (ChangedValueDTO a) = ChangedValueDTO (f a)
  fmap _ NothingChangedDTO = NothingChangedDTO

instance FromJSON a => FromJSON (EventFieldDTO a) where
  parseJSON (Object o) = do
    efChanged <- o .: "changed"
    if efChanged
      then do
        efValue <- o .: "value"
        return $ ChangedValueDTO efValue
      else return NothingChangedDTO
  parseJSON _ = mzero

instance ToJSON a => ToJSON (EventFieldDTO a) where
  toJSON (ChangedValueDTO efValue) = object ["changed" .= True, "value" .= efValue]
  toJSON NothingChangedDTO = object ["changed" .= False]

-- EventPathDTO
data EventPathItemDTO =
  EventPathItemDTO
    { _eventPathItemDTOPType :: String
    , _eventPathItemDTOUuid :: U.UUID
    }
  deriving (Show, Eq)

type EventPathDTO = [EventPathItemDTO]

instance FromJSON EventPathItemDTO where
  parseJSON (Object o) = do
    _eventPathItemDTOPType <- o .: "type"
    _eventPathItemDTOUuid <- o .: "uuid"
    return EventPathItemDTO {..}
  parseJSON _ = mzero

instance ToJSON EventPathItemDTO where
  toJSON EventPathItemDTO {..} = object ["type" .= _eventPathItemDTOPType, "uuid" .= _eventPathItemDTOUuid]

-- KnowledgeModelDTO
data MetricMeasureDTO =
  MetricMeasureDTO
    { _metricMeasureDTOMetricUuid :: U.UUID
    , _metricMeasureDTOMeasure :: Double
    , _metricMeasureDTOWeight :: Double
    }
  deriving (Show, Eq)

instance ToJSON MetricMeasureDTO where
  toJSON MetricMeasureDTO {..} =
    object
      [ "metricUuid" .= _metricMeasureDTOMetricUuid
      , "measure" .= _metricMeasureDTOMeasure
      , "weight" .= _metricMeasureDTOWeight
      ]

instance FromJSON MetricMeasureDTO where
  parseJSON (Object o) = do
    _metricMeasureDTOMetricUuid <- o .: "metricUuid"
    _metricMeasureDTOMeasure <- o .: "measure"
    _metricMeasureDTOWeight <- o .: "weight"
    return MetricMeasureDTO {..}
  parseJSON _ = mzero

-- KnowledgeModel
data QuestionValueType
  = StringQuestionValueType
  | NumberQuestionValueType
  | DateQuestionValueType
  | TextQuestionValueType
  deriving (Show, Eq, Generic)

-- Common
serializeQuestionValueType :: QuestionValueType -> String
serializeQuestionValueType questionType =
  case questionType of
    StringQuestionValueType -> "StringValue"
    NumberQuestionValueType -> "NumberValue"
    DateQuestionValueType -> "DateValue"
    TextQuestionValueType -> "TextValue"

deserializeQuestionValueType :: String -> Maybe QuestionValueType
deserializeQuestionValueType "StringValue" = Just StringQuestionValueType
deserializeQuestionValueType "NumberValue" = Just NumberQuestionValueType
deserializeQuestionValueType "DateValue" = Just DateQuestionValueType
deserializeQuestionValueType "TextValue" = Just TextQuestionValueType
deserializeQuestionValueType _ = Nothing

deserializeEventFieldQuestionValueType :: EventFieldDTO String -> EventFieldDTO QuestionValueType
deserializeEventFieldQuestionValueType (ChangedValueDTO "StringValue") = ChangedValueDTO StringQuestionValueType
deserializeEventFieldQuestionValueType (ChangedValueDTO "NumberValue") = ChangedValueDTO NumberQuestionValueType
deserializeEventFieldQuestionValueType (ChangedValueDTO "DateValue") = ChangedValueDTO DateQuestionValueType
deserializeEventFieldQuestionValueType (ChangedValueDTO "TextValue") = ChangedValueDTO TextQuestionValueType
deserializeEventFieldQuestionValueType _ = NothingChangedDTO
