module Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO

instance FromJSON QuestionnaireContentChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireContentChangeDTO where
  toJSON = genericToJSON simpleOptions
