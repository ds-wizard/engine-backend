module Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()

instance FromJSON QuestionnaireContentChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireContentChangeDTO where
  toJSON = genericToJSON simpleOptions
