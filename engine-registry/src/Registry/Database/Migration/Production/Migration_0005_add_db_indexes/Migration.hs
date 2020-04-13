module Registry.Database.Migration.Production.Migration_0005_add_db_indexes.Migration
  ( definition
  ) where

import Control.Monad.Logger
import qualified Data.Text as T
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5, mmName = "Add DB Indexes", mmDescription = "Add indexes for collection"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createIndex dbPool "organizations" "organizationId" True
  createIndex dbPool "organizations" "token" True
  createIndex dbPool "packages" "id" True
  return Nothing

createIndex dbPool collection field unique = do
  let i =
        [ "createIndexes" =: collection
        , "indexes" =:
          [["key" =: [field =: 1], "name" =: (T.append collection . T.append "_" $ field), "unique" =: unique]]
        ]
  runMongoDBPoolDef (runCommand i) dbPool
  return Nothing
