module Api.Resource.Common where

import Api.Resource.Event.EventFieldDTO
import Model.KnowledgeModel.KnowledgeModel

serializeQuestionType :: QuestionType -> String
serializeQuestionType questionType =
  case questionType of
    QuestionTypeOptions -> "options"
    QuestionTypeList -> "list"
    QuestionTypeString -> "string"
    QuestionTypeNumber -> "number"
    QuestionTypeDate -> "date"
    QuestionTypeText -> "text"

deserializeQuestionType :: String -> Maybe QuestionType
deserializeQuestionType "options" = Just QuestionTypeOptions
deserializeQuestionType "list" = Just QuestionTypeList
deserializeQuestionType "string" = Just QuestionTypeString
deserializeQuestionType "number" = Just QuestionTypeNumber
deserializeQuestionType "date" = Just QuestionTypeDate
deserializeQuestionType "text" = Just QuestionTypeText
deserializeQuestionType _ = Nothing

deserializeEventFieldQuestionType :: EventFieldDTO String -> EventFieldDTO QuestionType
deserializeEventFieldQuestionType (ChangedValueDTO "options") = ChangedValueDTO QuestionTypeOptions
deserializeEventFieldQuestionType (ChangedValueDTO "list") = ChangedValueDTO QuestionTypeList
deserializeEventFieldQuestionType (ChangedValueDTO "string") = ChangedValueDTO QuestionTypeString
deserializeEventFieldQuestionType (ChangedValueDTO "number") = ChangedValueDTO QuestionTypeNumber
deserializeEventFieldQuestionType (ChangedValueDTO "date") = ChangedValueDTO QuestionTypeDate
deserializeEventFieldQuestionType (ChangedValueDTO "text") = ChangedValueDTO QuestionTypeText
deserializeEventFieldQuestionType _ = NothingChangedDTO
