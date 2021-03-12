module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO

instance FromJSON QuestionnaireVersionChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireVersionChangeDTO where
  toJSON = genericToJSON simpleOptions
