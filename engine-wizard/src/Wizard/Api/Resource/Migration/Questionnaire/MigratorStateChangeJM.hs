module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()

instance FromJSON MigratorStateChangeDTO where
  parseJSON = simpleParseJSON "_migratorStateChangeDTO"

instance ToJSON MigratorStateChangeDTO where
  toJSON = simpleToJSON "_migratorStateChangeDTO"
