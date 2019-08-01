module Api.Resource.Migration.Questionnaire.MigratorStateJM where

import Data.Aeson

import Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorStateDTO where
  parseJSON = simpleParseJSON "_migratorStateDTO"

instance ToJSON MigratorStateDTO where
  toJSON = simpleToJSON "_migratorStateDTO"
