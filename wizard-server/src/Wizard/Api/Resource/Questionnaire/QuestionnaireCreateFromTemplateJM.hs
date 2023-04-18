module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO

instance FromJSON QuestionnaireCreateFromTemplateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireCreateFromTemplateDTO where
  toJSON = genericToJSON jsonOptions
