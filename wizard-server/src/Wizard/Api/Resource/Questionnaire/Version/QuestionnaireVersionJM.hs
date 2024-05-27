module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Model.Questionnaire.QuestionnaireVersion
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON QuestionnaireVersion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireVersion where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnaireVersionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireVersionDTO where
  toJSON = genericToJSON jsonOptions
