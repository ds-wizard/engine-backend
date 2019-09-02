module Api.Resource.Migration.KnowledgeModel.MigratorStateJM where

import Data.Aeson

import Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorStateDTO where
  parseJSON = simpleParseJSON "_migratorStateDTO"

instance ToJSON MigratorStateDTO where
  toJSON = simpleToJSON "_migratorStateDTO"
