module Wizard.Database.Migration.Production.Migration_0038_documentOwner_and_doc_perm.Migration
  ( definition
  ) where

import Control.Monad.Logger hiding (logInfo)
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 38
    , mmName = "Document Owner and 'DOC_PERM'"
    , mmDescription = "Rename 'ownerUuid' to 'creatorUuid' and add 'DOC_PERM' to admins"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addPermissionToAdmins dbPool
  renameDocumentOwnerUuid dbPool
  return Nothing

addPermissionToAdmins dbPool = do
  let action = modify (select ["role" =: "admin"] "users") ["$push" =: ["permissions" =: "DOC_PERM"]]
  runMongoDBPoolDef action dbPool

renameDocumentOwnerUuid dbPool = do
  let action = modify (select [] "documents") ["$rename" =: ["ownerUuid" =: "creatorUuid"]]
  runMongoDBPoolDef action dbPool
