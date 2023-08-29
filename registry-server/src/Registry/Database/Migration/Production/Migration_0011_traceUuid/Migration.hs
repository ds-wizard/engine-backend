module Registry.Database.Migration.Production.Migration_0011_traceUuid.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 11, mmName = "Trace Uuid", mmDescription = "Add traceUuid to PersistentCommand"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE persistent_command \
        \    ADD trace_uuid uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
