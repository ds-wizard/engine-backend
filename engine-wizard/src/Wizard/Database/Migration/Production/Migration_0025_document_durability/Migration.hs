module Wizard.Database.Migration.Production.Migration_0025_document_durability.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    {mmNumber = 25, mmName = "Document Durability", mmDescription = "Add new 'durability' field to Document"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action = modify (select [] "documents") ["$set" =: ["durability" =: ["_co" =: "PersistentDocumentDurability"]]]
  runMongoDBPoolDef action dbPool
  return Nothing
