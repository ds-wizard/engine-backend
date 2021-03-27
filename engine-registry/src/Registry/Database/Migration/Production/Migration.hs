module Registry.Database.Migration.Production.Migration
  ( runMigration
  ) where

import Control.Lens ((^.))
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Migration.Migration
import LensesConfig
import qualified Registry.Database.Migration.Production.Migration_0001_init.Migration as M_0001
import Registry.Util.Logger

runMigration baseContext = do
  migrateDatabase (baseContext ^. dbPool) migrationDefinitions (logInfo _CMP_MIGRATION)
  return ()

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions = [M_0001.definition]
