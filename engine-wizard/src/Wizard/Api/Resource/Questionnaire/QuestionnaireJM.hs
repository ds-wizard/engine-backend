module Wizard.Api.Resource.Questionnaire.QuestionnaireJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireStateJM ()
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON QuestionnaireDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireDTO where
  toJSON = genericToJSON simpleOptions
