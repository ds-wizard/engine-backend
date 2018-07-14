module Database.Migration.Production.Migration where

import Control.Lens ((^.))
import Database.MongoDB.Migration.Entity
import Database.MongoDB.Migration.Migration

import qualified
       Database.Migration.Production.Migration_0001_organizations_init.Migration
       as M_0001
import qualified
       Database.Migration.Production.Migration_0002_users_init.Migration
       as M_0002
import LensesConfig

runMigration appContext = do
  migrateDatabase (appContext ^. pool) migrationDefinitions
  return ()

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions = [M_0001.definition, M_0002.definition]
