module Wizard.Database.Migration.Production.Migration_0030_bson_generic.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 30, mmName = "BSON Generic", mmDescription = "Make (de)serialization generic"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  embedDocumentDurability dbPool "PersistentDocumentDurability"
  embedDocumentDurability dbPool "TemporallyDocumentDurability"
  return Nothing

embedDocumentDurability dbPool durability = do
  let action = modify (select ["durability._co" =: durability] "documents") ["$set" =: ["durability" =: durability]]
  runMongoDBPoolDef action dbPool
