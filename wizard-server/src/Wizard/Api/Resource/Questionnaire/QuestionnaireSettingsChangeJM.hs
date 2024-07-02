module Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeDTO

instance FromJSON QuestionnaireSettingsChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireSettingsChangeDTO where
  toJSON = genericToJSON jsonOptions
