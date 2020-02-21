module Wizard.Api.Resource.Questionnaire.QuestionnaireJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()

instance FromJSON QuestionnaireDTO where
  parseJSON = simpleParseJSON "_questionnaireDTO"

instance ToJSON QuestionnaireDTO where
  toJSON = simpleToJSON "_questionnaireDTO"
