module Wizard.Metamodel.Migration.Migration0001
  ( migrateEventValue
  ) where

import Data.Aeson
import qualified Data.Text as T

import Wizard.Metamodel.Migration.MigrationContext
import Wizard.Metamodel.Migration.Utils

-- Migration #0001 (KM v1 -> v2)
-- * Add integrations to KM
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

migrateEditKnowledgeModelEvent :: Object -> Object
migrateEditKnowledgeModelEvent = runBasicOp $ Insert "integrationUuids" unchangedValue

migrateEvents :: T.Text -> Object -> Object
migrateEvents "EditKnowledgeModelEvent" = migrateEditKnowledgeModelEvent
migrateEvents _ = id

migrate :: Value -> Value
migrate = migrateByEventType migrateEvents
