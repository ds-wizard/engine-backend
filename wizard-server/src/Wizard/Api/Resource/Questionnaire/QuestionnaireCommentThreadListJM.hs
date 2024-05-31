module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON QuestionnaireCommentThreadList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireCommentThreadList where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnaireCommentList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireCommentList where
  toJSON = genericToJSON jsonOptions
