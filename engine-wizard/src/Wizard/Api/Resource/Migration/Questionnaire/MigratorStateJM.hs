module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM where

import Data.Aeson

import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorStateDTO where
  parseJSON = simpleParseJSON "_migratorStateDTO"

instance ToJSON MigratorStateDTO where
  toJSON = simpleToJSON "_migratorStateDTO"
