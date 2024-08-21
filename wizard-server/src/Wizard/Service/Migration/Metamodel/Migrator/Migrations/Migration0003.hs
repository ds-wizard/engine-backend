module Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0003 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as V

import Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext
import Wizard.Service.Migration.Metamodel.Migrator.Migrations.Utils

-- Migration #0003 (KM v3 -> v4)
-- . Change "path" to "parentUuid"
-- . Change "<x>Uuid" to "entityUuid"
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [migrate input]

extractPathItemUuid :: Value -> Value
extractPathItemUuid v@(Object obj) =
  case KM.lookup "uuid" obj of
    (Just value) -> value
    _ -> nullUuid
extractPathItemUuid v = nullUuid

transformPathToParentUuid :: Value -> Value
transformPathToParentUuid (Array vec)
  | V.length vec > 0 = extractPathItemUuid $ V.last vec
transformPathToParentUuid x = nullUuid

migrateKnowledgeModelEvent :: Object -> Object
migrateKnowledgeModelEvent = runBasicOps [Rename "kmUuid" "entityUuid", Delete "path", Insert "parentUuid" nullUuid]

migrateAnyNonKmEvent :: Object -> Object
migrateAnyNonKmEvent = runBasicOps [Rename "path" "parentUuid", Change "parentUuid" transformPathToParentUuid]

migrateAnyAnswerEvent :: Object -> Object
migrateAnyAnswerEvent = runBasicOp $ Rename "answerUuid" "entityUuid"

migrateAnyChapterEvent :: Object -> Object
migrateAnyChapterEvent = runBasicOp $ Rename "chapterUuid" "entityUuid"

migrateAnyExpertEvent :: Object -> Object
migrateAnyExpertEvent = runBasicOp $ Rename "expertUuid" "entityUuid"

migrateAnyIntegrationEvent :: Object -> Object
migrateAnyIntegrationEvent = runBasicOp $ Rename "integrationUuid" "entityUuid"

migrateAnyQuestionEvent :: Object -> Object
migrateAnyQuestionEvent = runBasicOp $ Rename "questionUuid" "entityUuid"

migrateAnyReferenceEvent :: Object -> Object
migrateAnyReferenceEvent = runBasicOp $ Rename "referenceUuid" "entityUuid"

migrateAnyTagEvent :: Object -> Object
migrateAnyTagEvent = runBasicOp $ Rename "tagUuid" "entityUuid"

migrateEvents :: T.Text -> Object -> Object
migrateEvents "AddKnowledgeModelEvent" = migrateKnowledgeModelEvent
migrateEvents "EditKnowledgeModelEvent" = migrateKnowledgeModelEvent
migrateEvents "AddAnswerEvent" = migrateAnyAnswerEvent . migrateAnyNonKmEvent
migrateEvents "EditAnswerEvent" = migrateAnyAnswerEvent . migrateAnyNonKmEvent
migrateEvents "DeleteAnswerEvent" = migrateAnyAnswerEvent . migrateAnyNonKmEvent
migrateEvents "AddChapterEvent" = migrateAnyChapterEvent . migrateAnyNonKmEvent
migrateEvents "EditChapterEvent" = migrateAnyChapterEvent . migrateAnyNonKmEvent
migrateEvents "DeleteChapterEvent" = migrateAnyChapterEvent . migrateAnyNonKmEvent
migrateEvents "AddExpertEvent" = migrateAnyExpertEvent . migrateAnyNonKmEvent
migrateEvents "EditExpertEvent" = migrateAnyExpertEvent . migrateAnyNonKmEvent
migrateEvents "DeleteExpertEvent" = migrateAnyExpertEvent . migrateAnyNonKmEvent
migrateEvents "AddIntegrationEvent" = migrateAnyIntegrationEvent . migrateAnyNonKmEvent
migrateEvents "EditIntegrationEvent" = migrateAnyIntegrationEvent . migrateAnyNonKmEvent
migrateEvents "DeleteIntegrationEvent" = migrateAnyIntegrationEvent . migrateAnyNonKmEvent
migrateEvents "AddQuestionEvent" = migrateAnyQuestionEvent . migrateAnyNonKmEvent
migrateEvents "EditQuestionEvent" = migrateAnyQuestionEvent . migrateAnyNonKmEvent
migrateEvents "DeleteQuestionEvent" = migrateAnyQuestionEvent . migrateAnyNonKmEvent
migrateEvents "AddReferenceEvent" = migrateAnyReferenceEvent . migrateAnyNonKmEvent
migrateEvents "EditReferenceEvent" = migrateAnyReferenceEvent . migrateAnyNonKmEvent
migrateEvents "DeleteReferenceEvent" = migrateAnyReferenceEvent . migrateAnyNonKmEvent
migrateEvents "AddTagEvent" = migrateAnyTagEvent . migrateAnyNonKmEvent
migrateEvents "EditTagEvent" = migrateAnyTagEvent . migrateAnyNonKmEvent
migrateEvents "DeleteTagEvent" = migrateAnyTagEvent . migrateAnyNonKmEvent
migrateEvents _ = migrateAnyNonKmEvent

migrate :: Value -> Value
migrate = migrateByEventType migrateEvents
