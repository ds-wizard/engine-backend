module Wizard.Messaging.Resource.Questionnaire.QuestionnaireEventMJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireEventJM ()
import Wizard.Messaging.Resource.Questionnaire.QuestionnaireEventMDTO

instance FromJSON QuestionnaireEventMDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireEventMDTO where
  toJSON = genericToJSON simpleOptions
