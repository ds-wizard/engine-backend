module Registry.Database.Migration.Production.Migration
  ( runMigration
  ) where

import Control.Lens ((^.))
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Migration.Migration

import LensesConfig
import qualified Registry.Database.Migration.Production.Migration_0001_init.Migration as M_0001
import qualified Registry.Database.Migration.Production.Migration_0002_app.Migration as M_0002
import Registry.Util.Logger

runMigration baseContext = do
  let loggingLevel = baseContext ^. serverConfig . logging . level
  runLogging loggingLevel $ migrateDatabase (baseContext ^. dbPool) migrationDefinitions (logInfo _CMP_MIGRATION)

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions = [M_0001.definition, M_0002.definition]
