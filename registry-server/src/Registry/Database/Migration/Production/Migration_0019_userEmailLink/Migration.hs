module Registry.Database.Migration.Production.Migration_0019_userEmailLink.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 19, mmName = "User email link", mmDescription = "Rename action_key table to user_email_link and update link types"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  renameUserEmailLinkTable dbPool

renameUserEmailLinkTable dbPool = do
  let sql =
        "ALTER TABLE IF EXISTS action_key RENAME TO user_email_link; \
        \ALTER TABLE IF EXISTS user_email_link RENAME CONSTRAINT action_key_pk TO user_email_link_pk; \
        \UPDATE user_email_link SET type = 'RegistrationUserEmailLinkType' WHERE type = 'RegistrationActionKey'; \
        \UPDATE user_email_link SET type = 'ForgottenTokenUserEmailLinkType' WHERE type = 'ForgottenTokenActionKey';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
