module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Questionnaire.QuestionnaireVersion

instance FromJSON QuestionnaireVersion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireVersion where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnaireVersionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireVersionDTO where
  toJSON = genericToJSON jsonOptions
