module Registry.Database.Migration.Production.Migration_0009_persistentCommandDestination.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 9, mmName = "Persistent Command Destination", mmDescription = "Add destination to Persistent Command"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE persistent_command \
        \   ADD COLUMN destination varchar;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
