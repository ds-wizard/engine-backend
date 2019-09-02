module Api.Resource.Questionnaire.QuestionnaireCreateJM where

import Data.Aeson

import Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON QuestionnaireCreateDTO where
  parseJSON = simpleParseJSON "_questionnaireCreateDTO"

instance ToJSON QuestionnaireCreateDTO where
  toJSON = simpleToJSON "_questionnaireCreateDTO"
