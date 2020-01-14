module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictJM where

import Data.Aeson

import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationConflictActionJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON MigratorConflictDTO where
  parseJSON = simpleParseJSON "_migratorConflictDTO"

instance ToJSON MigratorConflictDTO where
  toJSON = simpleToJSON "_migratorConflictDTO"
