module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentListJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentListDTO

instance FromJSON QuestionnaireCommentListDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireCommentListDTO where
  toJSON = genericToJSON simpleOptions
