module Wizard.Metamodel.Migration.Migration0010
  ( migrateEventValue
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import Wizard.Metamodel.Migration.MigrationContext
import Wizard.Metamodel.Migration.Utils

-- Migration #0010 (KM v10 -> v11)
-- * Add "createdAt" to all events (value provided in MigrationContext)
-- * Change "annotations" in all events (from map to list)
-- * Change "requestHeaders" in integration events (from map to list)
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue ctx input = Right [migrate ctx input]

transformMapToList :: Value -> Value
transformMapToList (Object obj) = Array . V.fromList . map (uncurry tupleToEntry) . HM.toList $ obj
  where
    tupleToEntry :: T.Text -> Value -> Value
    tupleToEntry key value = Object . HM.insert "key" (String key) $ HM.singleton "value" value

migrateAnyEventAddCreatedAt :: Value -> Object -> Object
migrateAnyEventAddCreatedAt createdAt = runBasicOp (Insert "createdAt" createdAt)

migrateAnyAddEventChangeAnnotations :: Object -> Object
migrateAnyAddEventChangeAnnotations = runBasicOp (Change "annotations" transformMapToList)

migrateAnyEditEventChangeAnnotations :: Object -> Object
migrateAnyEditEventChangeAnnotations = runBasicOp (Change "annotations" (applyOnEventField transformMapToList))

migrateAddIntegrationEventChangeRqHeaders :: Object -> Object
migrateAddIntegrationEventChangeRqHeaders = runBasicOp (Change "requestHeaders" transformMapToList)

migrateEditIntegrationEventChangeRqHeaders :: Object -> Object
migrateEditIntegrationEventChangeRqHeaders = runBasicOp (Change "requestHeaders" (applyOnEventField transformMapToList))

runMigrationAnyEvent :: MigrationContext -> T.Text -> Object -> Object
runMigrationAnyEvent ctx _ = migrateAnyEventAddCreatedAt createdAtValue
  where
    createdAtValue = toJSON (ctxCreatedAt ctx)

runMigrationAnyAddEvent :: T.Text -> Object -> Object
runMigrationAnyAddEvent eventType
  | eventType `startsWith` "Add" = migrateAnyAddEventChangeAnnotations
  | otherwise = id

runMigrationAnyEditEvent :: T.Text -> Object -> Object
runMigrationAnyEditEvent eventType
  | eventType `startsWith` "Edit" = migrateAnyEditEventChangeAnnotations
  | otherwise = id

migrateIntegrationEvents :: T.Text -> Object -> Object
migrateIntegrationEvents "AddIntegrationEvent" = migrateAddIntegrationEventChangeRqHeaders
migrateIntegrationEvents "EditIntegrationEvent" = migrateEditIntegrationEventChangeRqHeaders
migrateIntegrationEvents _ = id

migrate :: MigrationContext -> Value -> Value
migrate ctx =
  chainMigrations
    [ migrateByEventType (runMigrationAnyEvent ctx)
    , migrateByEventType runMigrationAnyAddEvent
    , migrateByEventType runMigrationAnyEditEvent
    , migrateByEventType migrateIntegrationEvents
    ]
