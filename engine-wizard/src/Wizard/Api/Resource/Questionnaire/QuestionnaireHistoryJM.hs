module Wizard.Api.Resource.Questionnaire.QuestionnaireHistoryJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireHistoryDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()

instance FromJSON QuestionnaireHistoryDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireHistoryDTO where
  toJSON = genericToJSON simpleOptions
