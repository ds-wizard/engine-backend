module Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0015 (
  migrateEventValue,
) where

import Data.Aeson

import Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext

-- Migration #0015 (KM v15 -> v16)
-- (Nothing to be migrated, make type of question events - file question)
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [input]
