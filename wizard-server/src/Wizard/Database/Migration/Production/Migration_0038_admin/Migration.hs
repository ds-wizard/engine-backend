module Wizard.Database.Migration.Production.Migration_0038_admin.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 38, mmName = "Admin", mmDescription = "Add admin integration"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addAdminUrls dbPool
  addTraceUuidToPersistentCommand dbPool
  addSquashedToBranchData dbPool
  deleteMoveQuestionnaireCommentsToSeparateTableCommand dbPool

addAdminUrls dbPool = do
  let sql =
        "ALTER TABLE app \
        \    ADD admin_server_url varchar, \
        \    ADD admin_client_url varchar;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addTraceUuidToPersistentCommand dbPool = do
  let sql =
        "ALTER TABLE persistent_command \
        \    ADD last_trace_uuid uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addSquashedToBranchData dbPool = do
  let sql =
        "ALTER TABLE branch_data \
        \    ADD squashed bool not null default false;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

deleteMoveQuestionnaireCommentsToSeparateTableCommand dbPool = do
  let sql =
        "DELETE \
        \FROM persistent_command \
        \WHERE component = 'Questionnaire' \
        \  AND function = 'moveQuestionnaireCommentsToSeparateTable';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
