module Api.Resource.Common where

import Model.KnowledgeModel.KnowledgeModel

serializeQuestionType :: QuestionType -> String
serializeQuestionType questionType =
  case questionType of
    QuestionTypeOption -> "option"
    QuestionTypeList -> "list"
    QuestionString -> "string"
    QuestionNumber -> "number"
    QuestionDate -> "date"
    QuestionText -> "text"

deserializeQuestionType :: String -> Maybe QuestionType
deserializeQuestionType "option" = Just QuestionTypeOption
deserializeQuestionType "list" = Just QuestionTypeList
deserializeQuestionType "string" = Just QuestionString
deserializeQuestionType "number" = Just QuestionNumber
deserializeQuestionType "date" = Just QuestionDate
deserializeQuestionType "text" = Just QuestionText
deserializeQuestionType _ = Nothing
