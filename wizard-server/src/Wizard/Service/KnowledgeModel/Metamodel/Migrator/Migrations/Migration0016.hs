module Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Migration0016 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T

import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext
import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Utils

-- Migration #0016 (KM v16 -> v17)
-- . Add validations to value questions
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

migrateAddValueQuestionEvent :: Object -> Object
migrateAddValueQuestionEvent obj =
  case KM.lookup "questionType" obj of
    (Just (String "ValueQuestion")) ->
      runBasicOps
        [Insert "validations" AT.emptyArray]
        obj
    _ -> obj

migrateEditValueQuestionEvent :: Object -> Object
migrateEditValueQuestionEvent obj =
  case KM.lookup "questionType" obj of
    (Just (String "ValueQuestion")) ->
      runBasicOps
        [Insert "validations" unchangedValue]
        obj
    _ -> obj

runMigration :: T.Text -> Object -> Object
runMigration "AddQuestionEvent" obj = migrateAddValueQuestionEvent obj
runMigration "EditQuestionEvent" obj = migrateEditValueQuestionEvent obj
runMigration _ obj = obj

migrate :: Value -> Value
migrate = migrateByEventType runMigration
