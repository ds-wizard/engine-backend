module Wizard.Database.Migration.Production.Migration_0065_idToUuid.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 64, mmName = "Switch from ID to UUID", mmDescription = "Switch from ID to UUID for knowledge models and document templates"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  updateUserPermissions dbPool
  dropProjectActionTable dbPool
  dropProjectImporterTable dbPool
  changeDocumentTemplatePrimaryKeyFromIdToUuid dbPool
  recreatePersistentCommandFromDocumentTemplateAssetDeleteFunction dbPool

updateUserPermissions dbPool = do
  let sql = "UPDATE user_entity SET permissions = array_remove(array_remove(permissions, 'PRJ_ACTION_PERM'), 'PRJ_IMPORTER_PERM');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropProjectActionTable dbPool = do
  let sql = "DROP TABLE project_action;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropProjectImporterTable dbPool = do
  let sql = "DROP TABLE project_importer;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeDocumentTemplatePrimaryKeyFromIdToUuid dbPool = do
  let sql =
        "ALTER TABLE config_submission_service_supported_format DROP CONSTRAINT config_submission_service_supported_format_dt_id_fk; \
        \ALTER TABLE config_submission_service_supported_format DROP CONSTRAINT config_submission_service_supported_format_format_uuid_fk; \
        \ALTER TABLE document DROP CONSTRAINT document_document_template_id_fk; \
        \ALTER TABLE document DROP CONSTRAINT document_format_uuid_fk; \
        \ALTER TABLE project DROP CONSTRAINT project_document_template_id_fk; \
        \ALTER TABLE project DROP CONSTRAINT questionnaire_format_uuid_fk; \
        \ALTER TABLE document_template_asset DROP CONSTRAINT document_template_asset_document_template_id_fk; \
        \ALTER TABLE document_template_draft_data DROP CONSTRAINT document_template_draft_data_document_template_id_fk; \
        \ALTER TABLE document_template_file DROP CONSTRAINT document_template_file_document_template_id_fk; \
        \ALTER TABLE document_template_format DROP CONSTRAINT document_template_format_document_template_id_fk; \
        \ALTER TABLE document_template_format_step DROP CONSTRAINT document_template_format_step_document_template_id_fk; \
        \ALTER TABLE document_template_format_step DROP CONSTRAINT document_template_format_step_format_uuid_fk; \
        \ \
        \ALTER TABLE document_template DROP CONSTRAINT document_template_pk; \
        \ALTER TABLE document_template RENAME COLUMN id TO uuid; \
        \ALTER TABLE document_template ALTER COLUMN uuid TYPE uuid USING gen_random_uuid(); \
        \ALTER TABLE document_template ADD CONSTRAINT document_template_pk PRIMARY KEY (uuid); \
        \ \
        \ALTER TABLE config_submission_service_supported_format RENAME document_template_id TO document_template_uuid; \
        \UPDATE config_submission_service_supported_format SET document_template_uuid = document_template.uuid FROM document_template WHERE config_submission_service_supported_format.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND config_submission_service_supported_format.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE config_submission_service_supported_format ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE config_submission_service_supported_format ADD CONSTRAINT config_submission_service_supported_format_dt_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document RENAME document_template_id TO document_template_uuid; \
        \UPDATE document SET document_template_uuid = document_template.uuid FROM document_template WHERE document.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document ADD CONSTRAINT document_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE project RENAME document_template_id TO document_template_uuid; \
        \UPDATE project SET document_template_uuid = document_template.uuid FROM document_template WHERE project.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND project.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE project ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE project ADD CONSTRAINT project_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE SET NULL; \
        \ \
        \ALTER TABLE document_template_asset RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_asset SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_asset.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_asset.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_asset ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_asset ADD CONSTRAINT document_template_asset_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document_template_draft_data RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_draft_data SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_draft_data.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_draft_data.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_draft_data ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_draft_data ADD CONSTRAINT document_template_draft_data_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document_template_file RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_file SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_file.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_file.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_file ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_file ADD CONSTRAINT document_template_file_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document_template_format RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_format SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_format.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_format.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_format ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_format ADD CONSTRAINT document_template_format_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document_template_format_step RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_format_step SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_format_step.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_format_step.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_format_step ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_format_step ADD CONSTRAINT document_template_format_step_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ \
        \ALTER TABLE document_template_format DROP CONSTRAINT document_template_format_pk; \
        \ALTER TABLE document_template_format ADD CONSTRAINT document_template_format_pk PRIMARY KEY (document_template_uuid, uuid); \
        \ \
        \ALTER TABLE config_submission_service_supported_format ADD CONSTRAINT config_submission_service_supported_format_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE CASCADE; \
        \ALTER TABLE project ADD CONSTRAINT project_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE SET NULL; \
        \ALTER TABLE document ADD CONSTRAINT document_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE CASCADE; \
        \ALTER TABLE document_template_format_step ADD CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE CASCADE;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

recreatePersistentCommandFromDocumentTemplateAssetDeleteFunction dbPool = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_document_template_asset_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM create_persistent_command( \
        \            'document_template_asset', \
        \            'deleteFromS3', \
        \            jsonb_build_object('documentTemplateUuid', OLD.document_template_uuid, 'assetUuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
