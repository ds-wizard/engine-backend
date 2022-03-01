module Wizard.Database.Migration.Production.Migration_0014_appLimit.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 14, mmName = "App Limit", mmDescription = "Add app limits"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createAppLimit dbPool
  addFileSizeToDocument dbPool
  addFileSizeToTemplateAsset dbPool
  addQuestionnaireAclUpdateCascade dbPool
  addFlagsForDocWorkerToAppConfig dbPool
  addInternalFlagToPersistentCommand dbPool
  addMachineFlagToUser dbPool

createAppLimit dbPool = do
  let sql =
        "CREATE TABLE app_limit \
        \ ( \
        \     uuid              uuid              not null \
        \         constraint app_limit_pk \
        \             primary key, \
        \     users             integer,\
        \     active_users      integer,\
        \     knowledge_models  integer,\
        \     branches          integer,\
        \     templates         integer,\
        \     questionnaires    integer,\
        \     documents         integer,\
        \     storage           bigint,\
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null \
        \ ); \
        \  \
        \ CREATE UNIQUE INDEX app_limit_uuid_uindex \
        \     ON app_limit (uuid); \
        \  \
        \INSERT INTO app_limit \
        \SELECT uuid, null,null, null, null, null, null, null, null, created_at, updated_at FROM app;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addFileSizeToDocument dbPool = do
  let sql =
        "ALTER TABLE document \
        \     ADD file_size bigint;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addFileSizeToTemplateAsset dbPool = do
  let sql =
        "ALTER TABLE template_asset \
        \     ADD file_size bigint not null default 0;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addQuestionnaireAclUpdateCascade dbPool = do
  let sql =
        "ALTER TABLE questionnaire_acl_user \
        \    DROP CONSTRAINT questionnaire_acl_user_questionnaire_uuid_fk; \
        \  \
        \ALTER TABLE questionnaire_acl_user \
        \    ADD CONSTRAINT questionnaire_acl_user_questionnaire_uuid_fk \
        \        FOREIGN KEY (questionnaire_uuid) REFERENCES questionnaire \
        \            ON update cascade ON delete cascade;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addFlagsForDocWorkerToAppConfig dbPool = do
  let sql =
        "UPDATE app_config \
        \SET feature = feature::jsonb || '{\"pdfOnlyEnabled\": false, \"pdfWatermarkEnabled\": false}'::jsonb \
        \WHERE uuid IS NOT NULL"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addInternalFlagToPersistentCommand dbPool = do
  let sql =
        "ALTER TABLE persistent_command \
          \ADD column internal bool not null default true"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addMachineFlagToUser dbPool = do
  let sql =
        "ALTER TABLE user_entity \
          \ADD column machine bool not null default false"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addSystemUser dbPool = do
  let sql =
        "INSERT INTO user_entity \
        \VALUES ('00000000-0000-0000-0000-000000000000', \
        \        'System', \
        \        'User', \
        \        'system@example.com', \
        \        'pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4=', \
        \        null, '[\"internal\"]', \
        \        'admin',  \
        \        '{ADMIN_PERM,UM_PERM,KM_PERM,KM_UPGRADE_PERM,KM_PUBLISH_PERM,PM_READ_PERM,PM_WRITE_PERM,QTN_PERM,QTN_TML_PERM,DMP_PERM,CFG_PERM,SUBM_PERM,TML_PERM,DOC_PERM}', \
        \        true, \
        \        '[]', \
        \        null, \
        \        '[]', \
        \        '2018-01-20 00:00:00.000000 +00:00', \
        \        '2018-01-20 00:00:00.000000 +00:00', \
        \        '2018-01-25 00:00:00.000000 +00:00', \
        \        '00000000-0000-0000-0000-000000000000', \
        \        true);"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
