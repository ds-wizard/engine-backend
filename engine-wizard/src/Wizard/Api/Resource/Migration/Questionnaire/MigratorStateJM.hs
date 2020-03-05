module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()

instance FromJSON MigratorStateDTO where
  parseJSON = simpleParseJSON "_migratorStateDTO"

instance ToJSON MigratorStateDTO where
  toJSON = simpleToJSON "_migratorStateDTO"
