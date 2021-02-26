module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()

instance FromJSON QuestionnaireVersionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireVersionDTO where
  toJSON = genericToJSON simpleOptions
