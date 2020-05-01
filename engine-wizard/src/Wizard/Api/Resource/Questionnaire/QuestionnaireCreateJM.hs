module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO

instance FromJSON QuestionnaireCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireCreateDTO where
  toJSON = genericToJSON simpleOptions
