module Registry.Database.Migration.Production.Migration_0013_jsonb.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 13, mmName = "Jsonb", mmDescription = "Change type from json to jsonb"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE audit ALTER COLUMN instance_statistics TYPE jsonb USING instance_statistics::jsonb; \
        \ALTER TABLE document_template ALTER COLUMN allowed_packages TYPE jsonb USING allowed_packages::jsonb; \
        \ALTER TABLE document_template ALTER COLUMN formats TYPE jsonb USING formats::jsonb; \
        \ALTER TABLE package ALTER COLUMN events TYPE jsonb USING events::jsonb"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
