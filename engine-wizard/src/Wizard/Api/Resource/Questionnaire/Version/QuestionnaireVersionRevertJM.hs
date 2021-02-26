module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO

instance FromJSON QuestionnaireVersionRevertDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireVersionRevertDTO where
  toJSON = genericToJSON simpleOptions
