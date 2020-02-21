module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO

instance FromJSON QuestionnaireCreateDTO where
  parseJSON = simpleParseJSON "_questionnaireCreateDTO"

instance ToJSON QuestionnaireCreateDTO where
  toJSON = simpleToJSON "_questionnaireCreateDTO"
