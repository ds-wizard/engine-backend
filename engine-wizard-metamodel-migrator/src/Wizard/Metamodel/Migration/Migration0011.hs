module Wizard.Metamodel.Migration.Migration0011
  ( migrateEventValue
  ) where

import Data.Aeson
import qualified Data.Text as T

import Wizard.Metamodel.Migration.MigrationContext
import Wizard.Metamodel.Migration.Utils

migrateAddIntegrationEvent :: Object -> Object
migrateAddIntegrationEvent =
  runBasicOps
    [ Insert "emptySearch" (Bool True)
    , Insert "integrationType" (String "ApiIntegration")
    , Rename "responseItemUrl" "itemUrl"
    ]

migrateEditIntegrationEvent :: Object -> Object
migrateEditIntegrationEvent =
  runBasicOps
    [ Insert "emptySearch" unchangedValue
    , Insert "integrationType" (String "ApiIntegration")
    , Rename "responseItemUrl" "itemUrl"
    ]

runMigration :: T.Text -> Object -> Object
runMigration "AddIntegrationEvent" obj = migrateAddIntegrationEvent obj
runMigration "EditIntegrationEvent" obj = migrateEditIntegrationEvent obj
runMigration _ obj = obj

migrate :: Value -> Value
migrate = migrateByEventType runMigration

migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]
