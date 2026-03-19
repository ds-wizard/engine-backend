module Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Migration0019 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext
import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Utils

-- Migration #0019 (KM v19 -> v20)
-- - remove:
--   - AddApiLegacyIntegrationEvent, EditApiLegacyIntegrationEvent
--   - AddWidgetIntegrationEvent, EditWidgetIntegrationEvent
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ = Right . migrate

migrateIntegrationEvent :: Object -> [Object]
migrateIntegrationEvent obj =
  case KM.lookup "integrationType" obj of
    (Just (String "ApiLegacyIntegration")) -> []
    (Just (String "WidgetIntegration")) -> []
    _ -> [obj]

runMigration :: T.Text -> Object -> [Object]
runMigration "AddIntegrationEvent" obj = migrateIntegrationEvent obj
runMigration "EditIntegrationEvent" obj = migrateIntegrationEvent obj
runMigration _ obj = [obj]

migrate :: Value -> [Value]
migrate = migrateByEventTypeList runMigration
