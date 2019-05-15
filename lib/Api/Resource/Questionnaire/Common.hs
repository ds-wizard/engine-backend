module Api.Resource.Questionnaire.Common where

import Data.Aeson

import Model.Questionnaire.Questionnaire

serializeQuestionnaireAccessibility :: QuestionnaireAccessibility -> String
serializeQuestionnaireAccessibility PublicQuestionnaire = "PublicQuestionnaire"
serializeQuestionnaireAccessibility PrivateQuestionnaire = "PrivateQuestionnaire"
serializeQuestionnaireAccessibility PublicReadOnlyQuestionnaire = "PublicReadOnlyQuestionnaire"

deserializeQuestionnaireAccessibility :: String -> Maybe QuestionnaireAccessibility
deserializeQuestionnaireAccessibility "PublicQuestionnaire" = Just PublicQuestionnaire
deserializeQuestionnaireAccessibility "PrivateQuestionnaire" = Just PrivateQuestionnaire
deserializeQuestionnaireAccessibility "PublicReadOnlyQuestionnaire" = Just PublicReadOnlyQuestionnaire
deserializeQuestionnaireAccessibility _ = Nothing

hDeserializeQuestionnaireAccessibility o callback = do
  accessibilityS <- o .: "accessibility"
  case deserializeQuestionnaireAccessibility accessibilityS of
    (Just accessibility) -> callback accessibility
    Nothing -> fail "Unsupported accessibility"
