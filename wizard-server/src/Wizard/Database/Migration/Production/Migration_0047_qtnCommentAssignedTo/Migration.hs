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
  addAssignedTo dbPool
  addAwsSES dbPool

addAssignedTo dbPool = do
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

addAwsSES dbPool = do
  let sql =
        "ALTER TABLE instance_config_mail ALTER COLUMN host DROP NOT NULL; \
        \ALTER TABLE instance_config_mail ALTER COLUMN security DROP NOT NULL; \
        \ \
        \ALTER TABLE instance_config_mail RENAME COLUMN host TO smtp_host; \
        \ALTER TABLE instance_config_mail RENAME COLUMN port TO smtp_port; \
        \ALTER TABLE instance_config_mail RENAME COLUMN security TO smtp_security; \
        \ALTER TABLE instance_config_mail RENAME COLUMN username TO smtp_username; \
        \ALTER TABLE instance_config_mail RENAME COLUMN password TO smtp_password; \
        \ \
        \ALTER TABLE instance_config_mail ADD COLUMN provider TEXT NOT NULL DEFAULT('smtp'); \
        \ALTER TABLE instance_config_mail ADD COLUMN aws_access_key_id TEXT; \
        \ALTER TABLE instance_config_mail ADD COLUMN aws_secret_access_key TEXT; \
        \ALTER TABLE instance_config_mail ADD COLUMN aws_region TEXT;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
