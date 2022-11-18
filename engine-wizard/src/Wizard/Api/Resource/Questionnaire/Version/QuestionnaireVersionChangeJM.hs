module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO

instance FromJSON QuestionnaireVersionChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireVersionChangeDTO where
  toJSON = genericToJSON jsonOptions
