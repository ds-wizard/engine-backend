module Wizard.Database.Migration.Production.Migration_0011_app_2.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 11
    , mmName = "Finalize adding app"
    , mmDescription = "Allow to turn on app features, adjust foreign keys"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addCommentPermForEditorsIfNotExists dbPool
  addServerAndClientUrlToApp dbPool
  addFeatureColumnToAppConfig dbPool
  adjustForeignKeys dbPool

addCommentPermForEditorsIfNotExists dbPool = do
  let sql =
        "UPDATE questionnaire_acl_user \
            \SET perms = array_append(perms, 'COMMENT') \
            \WHERE 'EDIT' = ANY (perms) AND not ('COMMENT' = ANY (perms));"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addServerAndClientUrlToApp dbPool = do
  let sql =
        "ALTER TABLE app RENAME COLUMN client_domain TO client_url;\
        \ALTER TABLE app ADD server_url varchar NOT NULL DEFAULT 'https://server.example.com';"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addFeatureColumnToAppConfig dbPool = do
  let sql = "ALTER TABLE app_config ADD feature json NOT NULL DEFAULT '{\"clientCustomizationEnabled\": false }';"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

adjustForeignKeys dbPool = do
  let sql =
        "ALTER TABLE package \
         \    DROP CONSTRAINT package_pk CASCADE, \
         \    ADD CONSTRAINT package_pk PRIMARY KEY (id, app_uuid); \
         \ \
         \ALTER TABLE feedback \
         \    ADD CONSTRAINT feedback_package_id_fk \
         \        FOREIGN KEY (package_id, app_uuid) REFERENCES package (id, app_uuid); \
         \ \
         \ALTER TABLE questionnaire \
         \    ADD CONSTRAINT questionnaire_package_id_fk \
         \        FOREIGN KEY (package_id, app_uuid) REFERENCES package (id, app_uuid); \
         \ \
         \ALTER TABLE knowledge_model_migration \
         \    ADD CONSTRAINT knowledge_model_migration_branch_previous_package_id_fk \
         \        FOREIGN KEY (branch_previous_package_id, app_uuid) REFERENCES package (id, app_uuid); \
         \ \
         \ALTER TABLE knowledge_model_migration \
         \    ADD CONSTRAINT knowledge_model_migration_target_package_id_fk \
         \        FOREIGN KEY (target_package_id, app_uuid) REFERENCES package (id, app_uuid); \
         \ \
         \ALTER TABLE package \
         \    ADD CONSTRAINT package_previous_package_id_fk \
         \        FOREIGN KEY (previous_package_id, app_uuid) REFERENCES package (id, app_uuid); \
         \ \
         \ALTER TABLE branch \
         \    ADD CONSTRAINT branch_package_id_fk \
         \        FOREIGN KEY (previous_package_id, app_uuid) REFERENCES package (id, app_uuid);\
         \ \
         \ALTER TABLE template \
         \    DROP CONSTRAINT template_pk CASCADE, \
         \    ADD CONSTRAINT template_pk PRIMARY KEY (id, app_uuid); \
         \ \
         \ALTER TABLE template_asset \
         \    ADD CONSTRAINT template_asset_template_id_app_uuid_fk \
         \        FOREIGN KEY (template_id, app_uuid) REFERENCES template (id, app_uuid); \
         \ALTER TABLE template_asset \
         \    DROP CONSTRAINT template_asset_pk CASCADE, \
         \    ADD CONSTRAINT template_asset_pk PRIMARY KEY (uuid, app_uuid); \
         \DROP INDEX template_asset_uuid_uindex; \
         \CREATE UNIQUE INDEX template_asset_uindex ON template_asset (uuid, app_uuid); \
         \ \
         \ALTER TABLE template_file \
         \    ADD CONSTRAINT template_file_template_id_fk \
         \        FOREIGN KEY (template_id, app_uuid) REFERENCES template (id, app_uuid); \
         \ALTER TABLE template_file \
         \    DROP CONSTRAINT template_file_pk CASCADE, \
         \    ADD CONSTRAINT template_file_pk PRIMARY KEY (uuid, app_uuid); \
         \DROP INDEX template_file_uuid_uindex; \
         \CREATE UNIQUE INDEX template_file_uindex ON template_file (uuid, app_uuid); \
         \ \
         \ALTER TABLE questionnaire \
         \    ADD CONSTRAINT questionnaire_template_id_fk \
         \        FOREIGN KEY (template_id, app_uuid) REFERENCES template (id, app_uuid); \
         \ \
         \ALTER TABLE document \
         \    ADD CONSTRAINT document_template_id_fk \
         \        FOREIGN KEY (template_id, app_uuid) REFERENCES template (id, app_uuid); \
         \ \
         \DROP INDEX user_email_uindex; \
         \CREATE UNIQUE INDEX user_email_uindex \
         \    ON user_entity (email, app_uuid);\
         \ \
         \DROP INDEX package_id_uindex; \
         \CREATE UNIQUE INDEX package_id_uindex \
         \    ON package (id, app_uuid);\
         \ \
         \DROP INDEX template_id_uindex; \
         \CREATE UNIQUE INDEX template_id_uindex \
         \    ON template (id, app_uuid);"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
