module Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0004 (
  migrateEventValue,
) where

import Data.Aeson

import Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext

-- Migration #0004 (KM v4 -> v5)
-- (Nothing to be migrated, new "Move" events allowed)
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [input]
