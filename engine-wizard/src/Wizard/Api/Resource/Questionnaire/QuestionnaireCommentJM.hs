module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()

instance FromJSON QuestionnaireCommentThreadDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireCommentThreadDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON QuestionnaireCommentDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireCommentDTO where
  toJSON = genericToJSON simpleOptions
