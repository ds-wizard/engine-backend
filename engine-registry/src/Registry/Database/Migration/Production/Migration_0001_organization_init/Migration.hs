module Registry.Database.Migration.Production.Migration_0001_organization_init.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Registry.Database.Migration.Production.Migration_0001_organization_init.Data.Organizations

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 1, mmName = "Organizations Init", mmDescription = ""}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "organizations" (organization now)
  result <- runMongoDBPoolDef action dbPool
  return Nothing
