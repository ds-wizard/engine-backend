module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()

instance FromJSON MigratorStateChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON MigratorStateChangeDTO where
  toJSON = genericToJSON simpleOptions
