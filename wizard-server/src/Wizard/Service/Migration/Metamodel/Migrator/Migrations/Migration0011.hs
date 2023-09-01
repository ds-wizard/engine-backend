module Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0011 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Text as T

import Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext
import Wizard.Service.Migration.Metamodel.Migrator.Migrations.Utils

-- Migration #0011 (KM v11 -> v12)

-- * Add "integrationType" (older = "ApiIntegration") to integration events

-- * Add "requestEmptySearch" field to integration events

-- * Rename "responseItemUrl" to "itemUrl" in integration events
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

migrateAddIntegrationEvent :: Object -> Object
migrateAddIntegrationEvent =
  runBasicOps
    [ Insert "requestEmptySearch" (Bool True)
    , Insert "integrationType" (String "ApiIntegration")
    , Rename "responseItemUrl" "itemUrl"
    ]

migrateEditIntegrationEvent :: Object -> Object
migrateEditIntegrationEvent =
  runBasicOps
    [ Insert "requestEmptySearch" unchangedValue
    , Insert "integrationType" (String "ApiIntegration")
    , Rename "responseItemUrl" "itemUrl"
    ]

runMigration :: T.Text -> Object -> Object
runMigration "AddIntegrationEvent" obj = migrateAddIntegrationEvent obj
runMigration "EditIntegrationEvent" obj = migrateEditIntegrationEvent obj
runMigration _ obj = obj

migrate :: Value -> Value
migrate = migrateByEventType runMigration
