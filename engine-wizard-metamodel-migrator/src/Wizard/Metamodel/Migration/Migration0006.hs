module Wizard.Metamodel.Migration.Migration0006
  ( migrateEventValue
  ) where

import Data.Aeson
import qualified Data.Text as T

import Wizard.Metamodel.Migration.MigrationContext
import Wizard.Metamodel.Migration.Utils

-- Migration #0006 (KM v6 -> v7)
-- * Delete "name" to add/edit KM events
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

migrateKnowledgeModelEvents :: T.Text -> Object -> Object
migrateKnowledgeModelEvents "AddKnowledgeModelEvent" = runBasicOp (Delete "name")
migrateKnowledgeModelEvents "EditKnowledgeModelEvent" = runBasicOp (Delete "name")
migrateKnowledgeModelEvents _ = id

migrate :: Value -> Value
migrate = migrateByEventType migrateKnowledgeModelEvents
