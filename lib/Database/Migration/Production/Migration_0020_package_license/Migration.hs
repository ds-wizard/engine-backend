module Database.Migration.Production.Migration_0020_package_license.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 20, mmName = "Package License", mmDescription = "Add license field to packages"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  migratePackages dbPool
  migratePublicPackages dbPool

migratePackages dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select [] "packages")
          [ "$set" =:
            ["license" =: "## License\\n There should be some license. For changing it, publish a new version."]
          ]
  runMongoDBPoolDef action dbPool
  return Nothing

migratePublicPackages dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select [] "publicPackages")
          [ "$set" =:
            ["license" =: "## License\\n There should be some license. For changing it, publish a new version."]
          ]
  runMongoDBPoolDef action dbPool
  return Nothing
