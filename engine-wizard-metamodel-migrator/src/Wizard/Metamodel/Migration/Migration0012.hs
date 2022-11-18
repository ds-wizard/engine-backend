module Wizard.Metamodel.Migration.Migration0012 (
  migrateEventValue,
) where

import Data.Aeson

import Wizard.Metamodel.Migration.MigrationContext

-- Migration #0012 (KM v12 -> v13)
-- (Nothing to be migrated, new "value question" types allowed)
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [input]
