module Wizard.Metamodel.Migration.Migration0002 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Text as T

import Wizard.Metamodel.Migration.MigrationContext
import Wizard.Metamodel.Migration.Utils

-- Migration #0002 (KM v2 -> v3)

-- * Change "valueType" of question events
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

transformValueType :: Value -> Value
transformValueType (String "StringValue") = String "StringQuestionValueType"
transformValueType (String "NumberValue") = String "NumberQuestionValueType"
transformValueType (String "DateValue") = String "DateQuestionValueType"
transformValueType (String "TextValue") = String "TextQuestionValueType"
transformValueType value = value

migrateAddQuestionEvent :: Object -> Object
migrateAddQuestionEvent = runBasicOp $ Change "valueType" transformValueType

migrateEditQuestionEvent :: Object -> Object
migrateEditQuestionEvent = runBasicOp $ Change "valueType" (applyOnEventField transformValueType)

migrateIntegrationEvents :: T.Text -> Object -> Object
migrateIntegrationEvents "AddQuestionEvent" = migrateAddQuestionEvent
migrateIntegrationEvents "EditQuestionEvent" = migrateEditQuestionEvent
migrateIntegrationEvents _ = id

migrate :: Value -> Value
migrate = migrateByEventType migrateIntegrationEvents
