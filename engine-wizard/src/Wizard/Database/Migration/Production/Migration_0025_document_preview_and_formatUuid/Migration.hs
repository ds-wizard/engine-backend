module Wizard.Database.Migration.Production.Migration_0025_document_preview_and_formatUuid.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 25
    , mmName = "Document Preview and FormatUuid"
    , mmDescription =
        "Add new 'durability' and 'questionnaireRepliesHash' fields to Document; Convert 'format' to 'formatUuid'"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addDurabilityAndQuestionnaireRepliesHash dbPool
  convertFormats dbPool
  return Nothing

addDurabilityAndQuestionnaireRepliesHash dbPool = do
  let action =
        modify
          (select [] "documents")
          ["$set" =: ["durability" =: ["_co" =: "PersistentDocumentDurability"], "questionnaireRepliesHash" =: 0]]
  runMongoDBPoolDef action dbPool

convertFormats dbPool = do
  convertFormat dbPool "json" "d3e98eb6-344d-481f-8e37-6a67b6cd1ad2"
  convertFormat dbPool "html" "a9293d08-59a4-4e6b-ae62-7a6a570b031c"
  convertFormat dbPool "pdf" "68c26e34-5e77-4e15-9bf7-06ff92582257"
  convertFormat dbPool "latex" "dbc94579-40d7-42c3-975c-71e30d07778b"
  convertFormat dbPool "docx" "f4bd941a-dfbe-4226-a1fc-200fb5269311"
  convertFormat dbPool "odt" "15e53172-bbae-4a0c-a4d9-8f3ddf60e7b6"
  convertFormat dbPool "markdown" "f0533e48-f4c5-4af2-b2c1-5a47d4a247c0"

convertFormat dbPool format formatUuid = do
  let action =
        modify
          (select ["format" =: format] "documents")
          ["$set" =: ["formatUuid" =: formatUuid], "$unset" =: ["format" =: ""]]
  runMongoDBPoolDef action dbPool
