module Registry.Database.Migration.Production.Migration_0008_unification.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 8, mmName = "Unification", mmDescription = "Unify the libraries"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addActionKeyAppUuidColumn dbPool
  dropPersistentCommandCreatedByNotNull dbPool
  renameActionKeyUserIdColumn dbPool

dropPersistentCommandCreatedByNotNull dbPool = do
  let sql =
        "ALTER TABLE persistent_command \
        \   ALTER COLUMN created_by DROP NOT NULL;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameActionKeyUserIdColumn dbPool = do
  let sql =
        "ALTER TABLE action_key \
        \   RENAME COLUMN organization_id TO identity;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addActionKeyAppUuidColumn dbPool = do
  let sql =
        "ALTER TABLE action_key \
        \   ADD COLUMN app_uuid uuid not null default '00000000-0000-0000-0000-000000000000';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
