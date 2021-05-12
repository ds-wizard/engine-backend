module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Questionnaire.QuestionnaireVersion

instance FromJSON QuestionnaireVersion where
  parseJSON = simpleParseJSON "_questionnaireVersion"

instance ToJSON QuestionnaireVersion where
  toJSON = simpleToJSON "_questionnaireVersion"

instance FromJSON QuestionnaireVersionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireVersionDTO where
  toJSON = genericToJSON simpleOptions
