module Api.Resource.Common where

import Api.Resource.Event.EventFieldDTO
import Model.KnowledgeModel.KnowledgeModel

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
