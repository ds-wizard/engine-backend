module Api.Resource.Migration.KnowledgeModel.MigratorConflictJM where

import Data.Aeson

import Api.Resource.Migration.KnowledgeModel.MigrationConflictActionJM
       ()
import Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorConflictDTO where
  parseJSON = simpleParseJSON "_migratorConflictDTO"

instance ToJSON MigratorConflictDTO where
  toJSON = simpleToJSON "_migratorConflictDTO"
