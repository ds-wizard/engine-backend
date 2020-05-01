module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()

instance FromJSON MigratorStateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MigratorStateDTO where
  toJSON = genericToJSON simpleOptions
