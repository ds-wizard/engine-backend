module Wizard.Database.Migration.Production.Migration_0027_localePerm.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 27, mmName = "Fix Locale Perm", mmDescription = "Add LOC_PERM to all admins"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "UPDATE user_entity set permissions = permissions || '{LOC_PERM}' WHERE role = 'admin' AND NOT ('LOC_PERM' = ANY (permissions))"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
