module Wizard.Database.Migration.Production.Migration_0026_locale_3.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 26, mmName = "Locale 3", mmDescription = "Fix locale management"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  changeLocalePrimaryKey dbPool
  fixLocaleDescriptionAndReadme dbPool

changeLocalePrimaryKey dbPool = do
  let sql =
        "DROP INDEX locale_uuid_uindex; \
        \CREATE UNIQUE INDEX locale_uuid_uindex \
        \    ON locale (id, app_uuid); \
        \ALTER TABLE locale \
        \    DROP CONSTRAINT locale_pk; \
        \ALTER TABLE locale \
        \    ADD CONSTRAINT locale_pk \
        \        PRIMARY KEY (id, app_uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

fixLocaleDescriptionAndReadme dbPool = do
  let sql =
        "UPDATE locale \
        \SET description = 'Default English locale', \
        \    readme      = '# English Locale' \
        \WHERE id = 'wizard:default:1.0.0'"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
