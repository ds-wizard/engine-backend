module Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleJM ()
import Wizard.Model.Questionnaire.QuestionnaireFileList
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON QuestionnaireFileList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireFileList where
  toJSON = genericToJSON jsonOptions
