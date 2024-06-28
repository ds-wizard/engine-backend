module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadNotificationJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadNotification

instance FromJSON QuestionnaireCommentThreadNotification where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireCommentThreadNotification where
  toJSON = genericToJSON jsonOptions
