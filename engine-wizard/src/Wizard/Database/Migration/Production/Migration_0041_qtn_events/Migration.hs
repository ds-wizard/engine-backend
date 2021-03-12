module Wizard.Database.Migration.Production.Migration_0041_qtn_events.Migration
  ( definition
  ) where

import Control.Monad.Logger hiding (logInfo)
import Control.Monad.Reader (liftIO)
import qualified Data.Bson as BSON
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Shared.Util.Uuid

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 41, mmName = "Qtn Events", mmDescription = "Change qtn replies to events"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  updateQuestionnaires dbPool
  return Nothing

-- ------------------------------------------------------------------------------------------------
updateQuestionnaires dbPool = do
  let action = rest =<< find (select [] "questionnaires")
  questionnaires <- runMongoDBPoolDef action dbPool
  traverse_ (updateQuestionnaire dbPool) questionnaires
  addDocumentQuestionnaireEventUuid dbPool

updateQuestionnaire dbPool qtn = do
  let qtnUuid = extractUuid qtn
  let replies = extractReplies qtn
  events <- traverse convertReply replies
  let action =
        modify
          (select ["uuid" =: qtnUuid] "questionnaires")
          ["$set" =: ["events" =: events, "versions" =: ([] :: [String])], "$unset" =: ["replies" =: ""]]
  runMongoDBPoolDef action dbPool
  return Nothing

convertReply field = do
  uuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let reply = BSON.value field
  let updatedReply =
        case extractReplyType reply of
          "IntegrationReply" ->
            let replyValue = extractReplyValue reply
             in case extractReplyValueType replyValue of
                  "PlainValue" ->
                    BSON.Doc
                      [ "type" =: "IntegrationReply"
                      , "value" =: ["type" =: "PlainType", "value" =: extractReplyValueValue replyValue]
                      ]
                  "IntegrationValue" ->
                    BSON.Doc
                      [ "type" =: "IntegrationReply"
                      , "value" =:
                        [ "type" =: "IntegrationType"
                        , "value" =: extractReplyValueValue replyValue
                        , "id" =: extractReplyValueId replyValue
                        ]
                      ]
          _ -> reply
  return
    [ "type" BSON.=: "SetReplyEvent"
    , "uuid" BSON.=: U.toString uuid
    , "path" BSON.=: BSON.label field
    , "value" BSON.=: updatedReply
    , "createdBy" BSON.=: (Nothing :: Maybe String)
    , "createdAt" BSON.=: now
    ]

addDocumentQuestionnaireEventUuid dbPool = do
  let action = modify (select [] "documents") ["$set" =: ["questionnaireEventUuid" =: (Nothing :: Maybe String)]]
  runMongoDBPoolDef action dbPool

-- ------------------------------------------------
-- Extractor
-- ------------------------------------------------
extractUuid :: BSON.Document -> String
extractUuid = fromJust . BSON.lookup "uuid"

extractReplyType :: BSON.Value -> String
extractReplyType (BSON.Doc doc) = fromJust $ BSON.lookup "type" doc

extractReplyValue :: BSON.Value -> BSON.Document
extractReplyValue (BSON.Doc doc) = fromJust $ BSON.lookup "value" doc

extractReplyValueType :: BSON.Document -> String
extractReplyValueType = fromJust . BSON.lookup "type"

extractReplyValueValue :: BSON.Document -> String
extractReplyValueValue = fromJust . BSON.lookup "value"

extractReplyValueId :: BSON.Document -> String
extractReplyValueId = fromJust . BSON.lookup "id"

extractReplies :: BSON.Document -> BSON.Document
extractReplies = fromJust . BSON.lookup "replies"
