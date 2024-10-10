module Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Questionnaire.QuestionnaireFileSimple

instance FromJSON QuestionnaireFileSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireFileSimple where
  toJSON = genericToJSON jsonOptions
