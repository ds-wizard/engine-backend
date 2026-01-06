module Registry.Database.Migration.Production.Migration_0016_project.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 16, mmName = "Project Refactor", mmDescription = "Refactor project schema and related tables"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "ALTER TABLE audit RENAME COLUMN questionnaire_count TO project_count;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
