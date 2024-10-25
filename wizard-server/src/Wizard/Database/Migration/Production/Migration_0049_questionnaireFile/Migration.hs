module Wizard.Database.Migration.Production.Migration_0049_questionnaireFile.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 49, mmName = "Add Questionnaire File", mmDescription = "Add support for questionnaire file"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createQtnFileTable dbPool
  addQtnFileRole dbPool

createQtnFileTable dbPool = do
  let sql =
        "CREATE TABLE questionnaire_file \
        \( \
        \    uuid               uuid        NOT NULL, \
        \    file_name          varchar     NOT NULL, \
        \    content_type       varchar     NOT NULL, \
        \    file_size          bigint      NOT NULL, \
        \    questionnaire_uuid uuid        NOT NULL, \
        \    created_by         uuid, \
        \    tenant_uuid        uuid        NOT NULL, \
        \    created_at         timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_file_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_file_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_file_user_uuid_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addQtnFileRole dbPool = do
  let sql = "UPDATE user_entity set permissions = permissions || '{QTN_FILE_PERM}' WHERE role = 'admin';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addStateForTenant dbPool = do
  let sql = "UPDATE user_entity set permissions = permissions || '{QTN_FILE_PERM}' WHERE role = 'admin';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
