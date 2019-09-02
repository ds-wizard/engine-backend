module Api.Resource.Migration.Questionnaire.MigratorStateChangeJM where

import Data.Aeson

import Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorStateChangeDTO where
  parseJSON = simpleParseJSON "_migratorStateChangeDTO"

instance ToJSON MigratorStateChangeDTO where
  toJSON = simpleToJSON "_migratorStateChangeDTO"
