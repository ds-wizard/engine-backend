module Database.Migration.Production.Migration_0019_bson_hashmap.Migration
  ( definition
  ) where

import Control.Applicative ((<*>))
import Control.Monad
import Control.Monad.Logger
import Data.Bson.Generic
import Data.Map (Map, fromList, toList)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)
import Prelude hiding (lookup)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 19, mmName = "BSON HashMap", mmDescription = "Change BSON mapping of HashMap"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  migratePackages dbPool "packages" "events"
  migratePackages dbPool "publicPackages" "events"
  migratePackages dbPool "branches" "events"
  migratePackages dbPool "kmMigrations" "branchEvents"
  migratePackages dbPool "kmMigrations" "targetPackageEvents"
  migratePackages dbPool "kmMigrations" "resultEvents"

migratePackages :: ConnectionPool -> T.Text -> T.Text -> LoggingT IO (Maybe Error)
migratePackages dbPool collection eventFieldName = do
  let action = rest =<< find (select [] collection)
  packages <- runMongoDBPoolDef action dbPool
  forM_ packages (updatePackage dbPool collection eventFieldName)
  return Nothing

updatePackage :: ConnectionPool -> T.Text -> T.Text -> Document -> LoggingT IO (Maybe Error)
updatePackage dbPool collection eventFieldName package = do
  (Array events) <- look eventFieldName package
  let updatedEvents =
        fmap
          (updateAddEvent "AddQuestionEvent" "props" .
           updateEditEvent "EditQuestionEvent" "props" .
           updateAddEvent "AddIntegrationEvent" "requestHeaders" .
           updateEditEvent "EditIntegrationEvent" "requestHeaders" . convertToDoc)
          events
  let updated = merge [eventFieldName =: updatedEvents] package
  runMongoDBPoolDef (save collection updated) dbPool
  return Nothing

updateAddEvent :: T.Text -> T.Text -> Document -> Document
updateAddEvent eventName fieldName event =
  hIsEvent eventName event $
  hExtractDocument fieldName event event $ \docMap ->
    hExtractArray "map" docMap event $ \inDoc -> merge [fieldName =: (catMaybes . fmap convertMapEntry $ inDoc)] event

updateEditEvent :: T.Text -> T.Text -> Document -> Document
updateEditEvent eventName fieldName event =
  hIsEvent eventName event $
  hExtractDocument fieldName event event $ \docValue ->
    hExtractDocument "value" docValue event $ \docMap ->
      hExtractArray "map" docMap event $ \inDoc ->
        merge [fieldName =: ["changed" =: True, "value" =: (catMaybes . fmap convertMapEntry $ inDoc)]] event

-- -------------------------
-- Private
-- -------------------------
convertToDoc :: Value -> Document
convertToDoc (Doc event) = event

convertMapEntry :: Value -> Maybe Field
convertMapEntry (Doc doc) = (=:) <$> lookup "key" doc <*> look "value" doc

hExtractDocument fieldName obj event callback =
  case look fieldName obj of
    Just (Doc doc) -> callback doc
    Just (Array _) -> event
    Nothing -> event

hExtractArray fieldName obj event callback =
  case look fieldName obj of
    Just (Array doc) -> callback doc
    Nothing -> event

hIsEvent eventName event callback =
  if lookup "eventType" event == Just eventName
    then callback
    else event

-- -------------------------
-- Instances
-- -------------------------
instance ToBSON (Map String String) where
  toBSON m = ["map" =: toList m]

instance ToBSON (String, String) where
  toBSON (key, value) = ["key" =: key, "value" =: value]

instance FromBSON (Map String String) where
  fromBSON doc = do
    mList <- lookup "map" doc
    return $ fromList mList

instance FromBSON (String, String) where
  fromBSON doc = do
    key <- lookup "key" doc
    value <- lookup "value" doc
    return (key, value)
