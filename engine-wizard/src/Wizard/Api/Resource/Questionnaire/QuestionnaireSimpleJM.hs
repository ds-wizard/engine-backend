module Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.Questionnaire.QuestionnaireSimple

instance FromJSON QuestionnaireSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireSimple where
  toJSON = genericToJSON jsonOptions
