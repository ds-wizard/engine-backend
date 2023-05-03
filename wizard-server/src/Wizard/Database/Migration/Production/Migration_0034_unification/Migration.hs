module Wizard.Database.Migration.Production.Migration_0034_unification.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 34, mmName = "Unification", mmDescription = "Unify the libraries"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE action_key \
        \   RENAME COLUMN user_id TO identity;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
