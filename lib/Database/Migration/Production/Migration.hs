module Database.Migration.Production.Migration where

import Control.Lens ((^.))
import Database.MongoDB.Migration.Entity
import Database.MongoDB.Migration.Migration

import qualified
       Database.Migration.Production.Migration_0001_organization_init.Migration
       as M_0001
import qualified
       Database.Migration.Production.Migration_0002_users_init.Migration
       as M_0002
import qualified
       Database.Migration.Production.Migration_0003_book_references_init.Migration
       as M_0003
import qualified
       Database.Migration.Production.Migration_0004_metrics_init.Migration
       as M_0004
import qualified
       Database.Migration.Production.Migration_0005_levels_init.Migration
       as M_0005
import LensesConfig

runMigration appContext = do
  migrateDatabase (appContext ^. pool) migrationDefinitions
  return ()

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions = [M_0001.definition, M_0002.definition, M_0003.definition, M_0004.definition, M_0005.definition]
