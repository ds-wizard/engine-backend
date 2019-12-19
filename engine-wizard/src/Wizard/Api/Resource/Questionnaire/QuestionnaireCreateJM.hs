module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM where

import Data.Aeson

import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON QuestionnaireCreateDTO where
  parseJSON = simpleParseJSON "_questionnaireCreateDTO"

instance ToJSON QuestionnaireCreateDTO where
  toJSON = simpleToJSON "_questionnaireCreateDTO"
