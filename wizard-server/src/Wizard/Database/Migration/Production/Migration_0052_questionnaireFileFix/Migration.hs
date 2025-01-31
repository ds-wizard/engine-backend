module Wizard.Database.Migration.Production.Migration_0052_questionnaireFileFix.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 52, mmName = "Fix Questionnaire File perm", mmDescription = "Fix missing Questionnaire File perm when create admin"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "UPDATE user_entity SET permissions = permissions || '{QTN_FILE_PERM}' WHERE role = 'admin' AND 'QTN_FILE_PERM' != ALL (permissions);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
