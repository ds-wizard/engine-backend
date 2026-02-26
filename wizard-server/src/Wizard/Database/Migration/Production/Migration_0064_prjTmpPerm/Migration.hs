module Wizard.Database.Migration.Production.Migration_0064_prjTmpPerm.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 64, mmName = "Fix Project Template Permissions", mmDescription = "Fix the project template permissions in the user_entity table."}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "UPDATE user_entity \
        \SET permissions = array_replace(permissions, 'PJR_TML_PERM', 'PRJ_TML_PERM') \
        \WHERE 'PJR_TML_PERM' = ANY(permissions);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
