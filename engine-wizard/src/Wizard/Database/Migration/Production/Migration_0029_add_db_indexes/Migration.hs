module Wizard.Database.Migration.Production.Migration_0029_add_db_indexes.Migration
  ( definition
  ) where

import Control.Monad.Logger
import qualified Data.Text as T
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 29, mmName = "Add DB Indexes", mmDescription = "Add indexes for collection"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createIndex dbPool "bookReferences" "shortUuid" True
  createIndex dbPool "branches" "uuid" True
  createIndex dbPool "documents" "uuid" True
  createIndex dbPool "packages" "id" True
  createIndex dbPool "questionnaires" "uuid" True
  createIndex dbPool "users" "uuid" True
  createIndex dbPool "users" "email" False
  return Nothing

createIndex dbPool collection field unique = do
  let i =
        [ "createIndexes" =: collection
        , "indexes" =:
          [["key" =: [field =: 1], "name" =: (T.append collection . T.append "_" $ field), "unique" =: unique]]
        ]
  runMongoDBPoolDef (runCommand i) dbPool
  return Nothing
