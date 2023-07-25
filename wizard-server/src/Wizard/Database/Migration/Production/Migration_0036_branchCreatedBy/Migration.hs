module Wizard.Database.Migration.Production.Migration_0036_branchCreatedBy.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 36, mmName = "Branch Created By", mmDescription = "Remove not null constraint from createdBy"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE branch \
        \   ALTER COLUMN created_by DROP NOT NULL;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
