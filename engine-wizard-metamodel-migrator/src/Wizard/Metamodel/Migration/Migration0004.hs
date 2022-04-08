module Wizard.Metamodel.Migration.Migration0004
  ( migrateEventValue
  ) where

import Data.Aeson

import Wizard.Metamodel.Migration.MigrationContext

-- Migration #0004 (KM v4 -> v5)
-- (Nothing to be migrated, new "Move" events allowed)
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [input]
