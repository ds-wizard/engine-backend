module Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Migration0014 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext
import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Utils

-- Migration #0014 (KM v14 -> v15)
-- . Remove old resource page references
-- . Nothing is needed for new resource collections and pages or item select questions
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

migrateEditKnowledgeModelEvent :: Object -> Object
migrateEditKnowledgeModelEvent =
  runBasicOps
    [ Insert "resourceCollectionUuids" unchangedValue
    ]

migrateAddReferenceEvent :: Object -> Object
migrateAddReferenceEvent obj =
  case KM.lookup "referenceType" obj of
    (Just (String "ResourcePageReference")) ->
      runBasicOps
        [ Insert "resourcePageUuid" Null
        , Delete "shortUuid"
        ]
        obj
    _ -> obj

migrateEditReferenceEvent :: Object -> Object
migrateEditReferenceEvent obj =
  case KM.lookup "referenceType" obj of
    (Just (String "ResourcePageReference")) ->
      runBasicOps
        [ Insert "resourcePageUuid" unchangedValue
        , Delete "shortUuid"
        ]
        obj
    _ -> obj

runMigration :: T.Text -> Object -> Object
runMigration "EditKnowledgeModelEvent" obj = migrateEditKnowledgeModelEvent obj
runMigration "AddReferenceEvent" obj = migrateAddReferenceEvent obj
runMigration "EditReferenceEvent" obj = migrateEditReferenceEvent obj
runMigration _ obj = obj

migrate :: Value -> Value
migrate = migrateByEventType runMigration
