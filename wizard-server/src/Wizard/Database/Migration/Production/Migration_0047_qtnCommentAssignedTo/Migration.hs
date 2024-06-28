module Wizard.Database.Migration.Production.Migration_0047_qtnCommentAssignedTo.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 47, mmName = "Qtn Comment Assigned To", mmDescription = "Add assigned_to to questionnaire_comment_thread"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE questionnaire_comment_thread \
        \  ADD COLUMN assigned_to uuid; \
        \ALTER TABLE questionnaire_comment_thread \
        \  ADD CONSTRAINT questionnaire_comment_thread_assigned_to FOREIGN KEY (assigned_to, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid);\
        \ALTER TABLE questionnaire_comment_thread \
        \  ADD COLUMN assigned_by uuid; \
        \ALTER TABLE questionnaire_comment_thread \
        \  ADD CONSTRAINT questionnaire_comment_thread_assigned_by FOREIGN KEY (assigned_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_comment_thread \
        \  ADD COLUMN notification_required bool;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
