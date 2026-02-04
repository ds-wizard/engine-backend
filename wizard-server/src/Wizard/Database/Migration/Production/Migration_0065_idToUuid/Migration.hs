module Wizard.Database.Migration.Production.Migration_0065_idToUuid.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 64, mmName = "Add user plugins", mmDescription = "Add user plugins feature"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  updateUserPermissions dbPool
  dropProjectActionTable dbPool
  dropProjectImporterTable dbPool

updateUserPermissions dbPool = do
  let sql = "UPDATE user_entity SET permissions = array_remove(array_remove(permissions, 'PRJ_ACTION_PERM'), 'PRJ_IMPORTER_PERM');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropProjectActionTable dbPool = do
  let sql = "DROP TABLE project_action;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropProjectImporterTable dbPool = do
  let sql = "DROP TABLE project_importer;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
