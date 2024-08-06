module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadAssignedJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON QuestionnaireCommentThreadAssigned where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireCommentThreadAssigned where
  toJSON = genericToJSON jsonOptions
