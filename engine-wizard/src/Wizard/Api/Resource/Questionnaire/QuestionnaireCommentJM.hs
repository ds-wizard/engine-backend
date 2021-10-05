module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Questionnaire.QuestionnaireComment

instance ToJSON QuestionnaireCommentThread where
  toJSON = simpleToJSON "_questionnaireCommentThread"

instance FromJSON QuestionnaireCommentThread where
  parseJSON = simpleParseJSON "_questionnaireCommentThread"

instance ToJSON QuestionnaireComment where
  toJSON = simpleToJSON "_questionnaireComment"

instance FromJSON QuestionnaireComment where
  parseJSON = simpleParseJSON "_questionnaireComment"
