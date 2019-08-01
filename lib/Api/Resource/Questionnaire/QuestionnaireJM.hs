module Api.Resource.Questionnaire.QuestionnaireJM where

import Data.Aeson

import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON QuestionnaireDTO where
  parseJSON = simpleParseJSON "_questionnaireDTO"

instance ToJSON QuestionnaireDTO where
  toJSON = simpleToJSON "_questionnaireDTO"
