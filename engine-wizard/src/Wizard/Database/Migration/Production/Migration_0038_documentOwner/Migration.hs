module Wizard.Database.Migration.Production.Migration_0038_documentOwner.Migration
  ( definition
  ) where

import Control.Monad.Logger hiding (logInfo)
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 38, mmName = "Document Owner", mmDescription = "Rename 'ownerUuid' to 'creatorUuid'"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action = modify (select [] "documents") ["$rename" =: ["ownerUuid" =: "creatorUuid"]]
  runMongoDBPoolDef action dbPool
  return Nothing
