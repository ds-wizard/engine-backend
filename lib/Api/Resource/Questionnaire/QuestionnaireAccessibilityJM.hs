module Api.Resource.Questionnaire.QuestionnaireAccessibilityJM where

import Control.Monad
import Data.Aeson

import Model.Questionnaire.Questionnaire

instance FromJSON QuestionnaireAccessibility where
  parseJSON (String "PublicQuestionnaire") = return PublicQuestionnaire
  parseJSON (String "PrivateQuestionnaire") = return PrivateQuestionnaire
  parseJSON (String "PublicReadOnlyQuestionnaire") = return PublicReadOnlyQuestionnaire
  parseJSON _ = mzero

instance ToJSON QuestionnaireAccessibility where
  toJSON PublicQuestionnaire = "PublicQuestionnaire"
  toJSON PrivateQuestionnaire = "PrivateQuestionnaire"
  toJSON PublicReadOnlyQuestionnaire = "PublicReadOnlyQuestionnaire"
