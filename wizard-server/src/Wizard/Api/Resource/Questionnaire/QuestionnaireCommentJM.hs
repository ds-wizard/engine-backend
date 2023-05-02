module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()

instance FromJSON QuestionnaireCommentThreadDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireCommentThreadDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON QuestionnaireCommentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireCommentDTO where
  toJSON = genericToJSON jsonOptions
