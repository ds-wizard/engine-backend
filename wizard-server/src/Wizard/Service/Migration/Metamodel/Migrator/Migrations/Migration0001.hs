module Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0001 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Text as T

import Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext
import Wizard.Service.Migration.Metamodel.Migrator.Migrations.Utils

-- Migration #0001 (KM v1 -> v2)
-- . Add integrations to KM
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

migrateEditKnowledgeModelEvent :: Object -> Object
migrateEditKnowledgeModelEvent = runBasicOp $ Insert "integrationUuids" unchangedValue

migrateEvents :: T.Text -> Object -> Object
migrateEvents "EditKnowledgeModelEvent" = migrateEditKnowledgeModelEvent
migrateEvents _ = id

migrate :: Value -> Value
migrate = migrateByEventType migrateEvents
