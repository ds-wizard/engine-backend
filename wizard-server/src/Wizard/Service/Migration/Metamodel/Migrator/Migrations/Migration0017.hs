module Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0017 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Text as T

import Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext
import Wizard.Service.Migration.Metamodel.Migrator.Migrations.Utils

-- Migration #0017 (KM v17 -> v18)
-- . Change integrationType from ApiIntegration to ApiLegacyIntegration
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

changeIntegrationTypeValue :: Value -> Value
changeIntegrationTypeValue (String "ApiIntegration") = String "ApiLegacyIntegration"
changeIntegrationTypeValue value = value

migrateAddIntegrationEvent :: Object -> Object
migrateAddIntegrationEvent =
  runBasicOps
    [ Change "integrationType" changeIntegrationTypeValue
    , Rename "props" "variables"
    ]

migrateEditIntegrationEvent :: Object -> Object
migrateEditIntegrationEvent =
  runBasicOps
    [ Change "integrationType" changeIntegrationTypeValue
    , Rename "props" "variables"
    ]

migrateAddQuestionEvent :: Object -> Object
migrateAddQuestionEvent =
  runBasicOps
    [ Rename "props" "variables"
    ]

migrateEditQuestionEvent :: Object -> Object
migrateEditQuestionEvent =
  runBasicOps
    [ Rename "props" "variables"
    ]

runMigration :: T.Text -> Object -> Object
runMigration "AddIntegrationEvent" obj = migrateAddIntegrationEvent obj
runMigration "EditIntegrationEvent" obj = migrateEditIntegrationEvent obj
runMigration "AddQuestionEvent" obj = migrateAddQuestionEvent obj
runMigration "EditQuestionEvent" obj = migrateEditQuestionEvent obj
runMigration _ obj = obj

migrate :: Value -> Value
migrate = migrateByEventType runMigration
