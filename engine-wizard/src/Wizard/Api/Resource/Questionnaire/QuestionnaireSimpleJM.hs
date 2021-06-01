module Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Questionnaire.QuestionnaireSimple

instance FromJSON QuestionnaireSimple where
  parseJSON = simpleParseJSON "_questionnaireSimple"

instance ToJSON QuestionnaireSimple where
  toJSON = simpleToJSON "_questionnaireSimple"
