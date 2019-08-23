module Database.Migration.Production.Migration_0018_package_readme_and_createdAt.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
  { mmNumber = 18
  , mmName = "Package Readme and CreatedAt"
  , mmDescription = "Add readme and createdAt field to Package entity"
  }

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
            ["readme" =: "**There should be README. For changing it, publish a new version.**", "createdAt" =: now]
          ]
  runMongoDBPoolDef action dbPool
  return Nothing

migratePublicPackages dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select [] "publicPackages")
          [ "$set" =:
            ["readme" =: "**There should be README. For changing it, publish a new version.**", "createdAt" =: now]
          ]
  runMongoDBPoolDef action dbPool
  return Nothing
