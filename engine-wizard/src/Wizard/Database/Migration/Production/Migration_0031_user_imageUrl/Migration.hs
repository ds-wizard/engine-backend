module Wizard.Database.Migration.Production.Migration_0031_user_imageUrl.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 31, mmName = "User ImageURL", mmDescription = "Add 'imageUrl' to User"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action = modify (select [] "users") ["$set" =: ["imageUrl" =: (Nothing :: Maybe String)]]
  runMongoDBPoolDef action dbPool
  return Nothing
