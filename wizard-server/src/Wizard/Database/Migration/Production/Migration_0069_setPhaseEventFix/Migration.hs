module Wizard.Database.Migration.Production.Migration_0069_setPhaseEventFix.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 69, mmName = "Fix set phase event", mmDescription = "Repair SetPhaseEvent project events with a NULL array element in value and remove the project migration walkthrough"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  fixSetPhaseEvents dbPool
  dropProjectMigrations dbPool

fixSetPhaseEvents dbPool = do
  let sql = "UPDATE project_event SET value = '{}' WHERE event_type = 'SetPhaseEvent' AND array_position(value, NULL) IS NOT NULL;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropProjectMigrations dbPool = do
  let sql =
        "DELETE FROM project WHERE uuid IN (SELECT new_project_uuid FROM project_migration); \
        \DROP TABLE project_migration;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
