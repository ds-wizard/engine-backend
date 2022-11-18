module Wizard.Metamodel.Migration.Migration0008 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import Wizard.Metamodel.Migration.MigrationContext
import Wizard.Metamodel.Migration.Utils

-- Migration #0008 (KM v8 -> v9)

-- * Add "annotations" to all add events (empty object)

-- * Add "annotations" to all edit events (no change)
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

emptyAnnotations :: Value
emptyAnnotations = Object KM.empty

migrateAnyAddEventChangeAnnotations :: Object -> Object
migrateAnyAddEventChangeAnnotations = runBasicOp (Insert "annotations" emptyAnnotations)

migrateAnyEditEventChangeAnnotations :: Object -> Object
migrateAnyEditEventChangeAnnotations = runBasicOp (Insert "annotations" unchangedValue)

runMigrationAnyAddEvent :: T.Text -> Object -> Object
runMigrationAnyAddEvent eventType
  | eventType `startsWith` "Add" = migrateAnyAddEventChangeAnnotations
  | otherwise = id

runMigrationAnyEditEvent :: T.Text -> Object -> Object
runMigrationAnyEditEvent eventType
  | eventType `startsWith` "Edit" = migrateAnyEditEventChangeAnnotations
  | otherwise = id

migrate :: Value -> Value
migrate = chainMigrations [migrateByEventType runMigrationAnyAddEvent, migrateByEventType runMigrationAnyEditEvent]
