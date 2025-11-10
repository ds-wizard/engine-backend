module Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Migration0013 (
  migrateEventValue,
) where

import Data.Aeson

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext

-- Migration #0013 (KM v13 -> v14)
-- (Nothing to be migrated, make integration fields optional)
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [input]
