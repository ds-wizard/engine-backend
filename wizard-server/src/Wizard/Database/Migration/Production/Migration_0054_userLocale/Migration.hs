module Wizard.Database.Migration.Production.Migration_0054_userLocale.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 54, mmName = "Add user Locale", mmDescription = "Add support for saving locale to user entity"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addUserLocale dbPool
  deleteAllLocalesExceptDefault dbPool

addUserLocale dbPool = do
  let sql =
        "ALTER TABLE user_entity ADD COLUMN locale VARCHAR; \
        \ALTER TABLE user_entity ADD CONSTRAINT user_entity_locale_fk FOREIGN KEY (locale, tenant_uuid) REFERENCES locale(id, tenant_uuid) ON DELETE SET DEFAULT;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

deleteAllLocalesExceptDefault dbPool = do
  let sql = "DELETE FROM locale WHERE id != 'wizard:default:1.0.0'"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
