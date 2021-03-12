module Wizard.Database.Migration.Production.Migration_0042_template_perm.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta {mmNumber = 42, mmName = "Template Perm", mmDescription = "Add TML_PERM to dataSteward"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action = modify (select ["role" =: "dataSteward"] "users") ["$push" =: ["permissions" =: "TML_PERM"]]
  runMongoDBPoolDef action dbPool
  return Nothing

