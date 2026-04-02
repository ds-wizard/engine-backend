module Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Migration0019 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (maybeToList)
import qualified Data.Text as T

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext
import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Utils

-- Migration #0019 (KM v19 -> v20)
-- - remove:
--   - AddApiLegacyIntegrationEvent, EditApiLegacyIntegrationEvent
--   - AddWidgetIntegrationEvent, EditWidgetIntegrationEvent
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ = Right . migrate

migrateEditIntegrationEventLegacy :: Object -> Maybe Object
migrateEditIntegrationEventLegacy obj =
  let name = extractValue "name" obj
      variables = extractValue "variables" obj
      requestUrl = extractValue "requestUrl" obj
      requestBody = extractValue "requestBody" obj
      requestMethod = extractValue "requestMethod" obj
      requestHeaders = extractValue "requestHeaders" obj
      requestAllowEmptySearch = extractValue "requestEmptySearch" obj
      responseListField = extractValue "responseListField" obj
      responseItemTemplate = extractValue "responseItemTemplate" obj
      annotations = extractValue "annotations" obj
   in Just $
        KM.fromList
          [ ("name", name)
          , ("eventType", String "EditIntegrationEvent")
          , ("integrationType", String "ApiIntegration")
          , ("allowCustomReply", unchangedValue)
          , ("variables", variables)
          , ("requestUrl", requestUrl)
          , ("requestBody", requestBody)
          , ("requestMethod", requestMethod)
          , ("requestHeaders", requestHeaders)
          , ("requestAllowEmptySearch", requestAllowEmptySearch)
          , ("responseListField", responseListField)
          , ("responseItemTemplate", responseItemTemplate)
          , ("responseItemTemplateForSelection", unchangedValue)
          , ("testQ", unchangedValue)
          , ("testVariables", unchangedValue)
          , ("testResponse", unchangedValue)
          , ("annotations", annotations)
          ]

migrateEditIntegrationEvent :: Object -> Maybe Object
migrateEditIntegrationEvent obj =
  case KM.lookup "integrationType" obj of
    (Just (String "ApiLegacyIntegration")) -> migrateEditIntegrationEventLegacy obj
    (Just (String "WidgetIntegration")) -> Nothing
    _ -> Just obj

migrateAddIntegrationEventLegacy :: Object -> Maybe Object
migrateAddIntegrationEventLegacy obj =
  let name = extractValue "name" obj
      variables = extractValue "variables" obj
      requestUrl = extractValue "requestUrl" obj
      requestBody = extractValue "requestBody" obj
      requestMethod = extractValue "requestMethod" obj
      requestHeaders = extractValue "requestHeaders" obj
      requestAllowEmptySearch = extractValue "requestEmptySearch" obj
      responseListField = extractValue "responseListField" obj
      responseItemTemplate = extractValue "responseItemTemplate" obj
      annotations = extractValue "annotations" obj
   in Just $
        KM.fromList
          [ ("name", name)
          , ("eventType", String "AddIntegrationEvent")
          , ("integrationType", String "ApiIntegration")
          , ("allowCustomReply", Bool True)
          , ("variables", variables)
          , ("requestUrl", requestUrl)
          , ("requestBody", requestBody)
          , ("requestMethod", requestMethod)
          , ("requestHeaders", requestHeaders)
          , ("requestAllowEmptySearch", requestAllowEmptySearch)
          , ("responseListField", responseListField)
          , ("responseItemTemplate", responseItemTemplate)
          , ("responseItemTemplateForSelection", Null)
          , ("testQ", String "")
          , ("testVariables", Object mempty)
          , ("testResponse", Null)
          , ("annotations", annotations)
          ]

migrateAddIntegrationEvent :: Object -> Maybe Object
migrateAddIntegrationEvent obj =
  case KM.lookup "integrationType" obj of
    (Just (String "ApiLegacyIntegration")) -> migrateAddIntegrationEventLegacy obj
    (Just (String "WidgetIntegration")) -> Nothing
    _ -> Just obj

runMigration :: T.Text -> Object -> Maybe Object
runMigration "AddIntegrationEvent" obj = migrateAddIntegrationEvent obj
runMigration "EditIntegrationEvent" obj = migrateEditIntegrationEvent obj
runMigration _ obj = Just obj

migrateContent :: Value -> Maybe Value
migrateContent = migrateByEventTypeMaybe runMigration

migrate :: Value -> [Value]
migrate = maybeToList . migrateEventContentMaybe migrateContent
