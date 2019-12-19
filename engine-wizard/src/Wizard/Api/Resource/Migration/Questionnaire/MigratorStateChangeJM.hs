module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeJM where

import Data.Aeson

import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorStateChangeDTO where
  parseJSON = simpleParseJSON "_migratorStateChangeDTO"

instance ToJSON MigratorStateChangeDTO where
  toJSON = simpleToJSON "_migratorStateChangeDTO"
