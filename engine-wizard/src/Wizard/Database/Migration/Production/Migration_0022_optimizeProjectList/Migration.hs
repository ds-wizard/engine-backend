module Wizard.Database.Migration.Production.Migration_0022_optimizeProjectList.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 22, mmName = "Optimize project list", mmDescription = "Optimize project list endpoint"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE questionnaire \
        \    ADD answered_questions int not null default 0, \
        \    ADD unanswered_questions int not null default 0"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
