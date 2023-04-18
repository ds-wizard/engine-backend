module WizardLib.Metamodel.Migration.Migration0009 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Text as T

import WizardLib.Metamodel.Migration.MigrationContext
import WizardLib.Metamodel.Migration.Utils

-- Migration #0009 (KM v9 -> v10)

-- * Rename "responseIdField" to "responseItemId" in integration events

-- * Rename "responseNameField" to "responseItemTemplate" in integration events

-- * Change "responseItemId" in integration events (from path to Jinja)

-- * Change "responseItemTemplate" in integration events (from path to Jinja)
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

navToTemplate :: T.Text -> T.Text
navToTemplate txt = "{{item." `T.append` txt `T.append` "}}"

transformStringToJinja :: Value -> Value
transformStringToJinja (String txt) = String $ navToTemplate txt
transformStringToJinja v = v

migrateAddIntegrationEvent :: Object -> Object
migrateAddIntegrationEvent =
  runBasicOps
    [ Rename "responseIdField" "responseItemId"
    , Rename "responseNameField" "responseItemTemplate"
    , Change "responseItemId" transformStringToJinja
    , Change "responseItemTemplate" transformStringToJinja
    ]

migrateEditIntegrationEvent :: Object -> Object
migrateEditIntegrationEvent =
  runBasicOps
    [ Rename "responseIdField" "responseItemId"
    , Rename "responseNameField" "responseItemTemplate"
    , Change "responseItemId" (applyOnEventField transformStringToJinja)
    , Change "responseItemTemplate" (applyOnEventField transformStringToJinja)
    ]

migrateIntegrationEvents :: T.Text -> Object -> Object
migrateIntegrationEvents "AddIntegrationEvent" = migrateAddIntegrationEvent
migrateIntegrationEvents "EditIntegrationEvent" = migrateEditIntegrationEvent
migrateIntegrationEvents _ = id

migrate :: Value -> Value
migrate = migrateByEventType migrateIntegrationEvents
