module Registry.Database.Migration.Production.Migration_0017_locale.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 17, mmName = "Locale Refactor", mmDescription = "Switch from id to uuid"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "DROP INDEX locale_uuid_uindex; \
        \ALTER TABLE locale DROP CONSTRAINT locale_pk; \
        \ALTER TABLE locale RENAME COLUMN id TO uuid; \
        \ALTER TABLE locale ALTER COLUMN uuid TYPE uuid USING gen_random_uuid(); \
        \ALTER TABLE locale ADD CONSTRAINT locale_pk PRIMARY KEY (uuid); \
        \CREATE UNIQUE INDEX locale_organization_id_locale_id_version_uindex ON locale (organization_id, locale_id, version);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
