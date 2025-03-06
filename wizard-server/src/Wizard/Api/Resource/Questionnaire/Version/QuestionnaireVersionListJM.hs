module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON QuestionnaireVersion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireVersion where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnaireVersionList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireVersionList where
  toJSON = genericToJSON jsonOptions
