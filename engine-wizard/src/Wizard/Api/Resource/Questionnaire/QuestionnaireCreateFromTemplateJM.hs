module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO

instance FromJSON QuestionnaireCreateFromTemplateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireCreateFromTemplateDTO where
  toJSON = genericToJSON simpleOptions
