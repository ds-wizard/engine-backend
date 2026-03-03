module Registry.Database.Migration.Production.Migration_0018_idToUuid.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 18, mmName = "Switch from ID to UUID", mmDescription = "Switch from ID to UUID for knowledge models and document templates"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  changeDocumentTemplatePrimaryKeyFromIdToUuid dbPool
  changeKnowledgeModelPrimaryKeyFromIdToUuid dbPool

changeDocumentTemplatePrimaryKeyFromIdToUuid dbPool = do
  let sql =
        "ALTER TABLE document_template_asset DROP CONSTRAINT template_asset_template_id_fk; \
        \ALTER TABLE document_template_file DROP CONSTRAINT document_template_file_template_id_fk; \
        \ALTER TABLE document_template_format DROP CONSTRAINT document_template_format_document_template_id_fk; \
        \ALTER TABLE document_template_format_step DROP CONSTRAINT document_template_format_step_document_template_id_fk; \
        \ALTER TABLE document_template_format_step DROP CONSTRAINT document_template_format_step_format_uuid_fk; \
        \ \
        \ALTER TABLE document_template DROP CONSTRAINT document_template_pk; \
        \ALTER TABLE document_template RENAME COLUMN id TO uuid; \
        \ALTER TABLE document_template ALTER COLUMN uuid TYPE uuid USING gen_random_uuid(); \
        \ALTER TABLE document_template ADD CONSTRAINT document_template_pk PRIMARY KEY (uuid); \
        \ \
        \ALTER TABLE document_template_asset RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_asset SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_asset.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_asset.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_asset ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_asset ADD CONSTRAINT document_template_asset_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
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
        \ALTER TABLE document_template_format_step ADD CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE CASCADE;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeKnowledgeModelPrimaryKeyFromIdToUuid dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_package_event DROP CONSTRAINT knowledge_model_package_event_package_id_fk; \
        \ALTER TABLE knowledge_model_package_event DROP CONSTRAINT knowledge_model_package_event_pk; \
        \ \
        \DROP INDEX package_previous_package_id_index; \
        \DROP INDEX package_id_uindex; \
        \DROP INDEX package_organization_id_km_id_index; \
        \ALTER TABLE knowledge_model_package DROP CONSTRAINT package_pk; \
        \ \
        \ \
        \ALTER TABLE knowledge_model_package_event RENAME COLUMN package_id TO package_uuid; \
        \ \
        \ALTER TABLE knowledge_model_package RENAME COLUMN previous_package_id TO previous_package_uuid; \
        \ALTER TABLE knowledge_model_package RENAME COLUMN id TO uuid; \
        \ \
        \ \
        \ALTER TABLE knowledge_model_package ALTER COLUMN uuid TYPE uuid USING gen_random_uuid(); \
        \ALTER TABLE knowledge_model_package ADD CONSTRAINT knowledge_model_package_pk PRIMARY KEY (uuid); \
        \ \
        \UPDATE knowledge_model_package SET previous_package_uuid = previous_pkg.uuid FROM knowledge_model_package previous_pkg WHERE knowledge_model_package.previous_package_uuid = previous_pkg.organization_id || ':' || previous_pkg.km_id || ':' || previous_pkg.version AND knowledge_model_package.tenant_uuid = previous_pkg.tenant_uuid; \
        \ALTER TABLE knowledge_model_package ALTER COLUMN previous_package_uuid TYPE uuid USING previous_package_uuid::uuid; \
        \ALTER TABLE knowledge_model_package ADD CONSTRAINT knowledge_model_package_previous_package_uuid_fk FOREIGN KEY (previous_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE ; \
        \ \
        \UPDATE knowledge_model_package_event SET package_uuid = knowledge_model_package.uuid FROM knowledge_model_package WHERE knowledge_model_package_event.package_uuid = knowledge_model_package.organization_id || ':' || knowledge_model_package.km_id || ':' || knowledge_model_package.version AND knowledge_model_package_event.tenant_uuid = knowledge_model_package.tenant_uuid; \
        \ALTER TABLE knowledge_model_package_event ALTER COLUMN package_uuid TYPE uuid USING package_uuid::uuid; \
        \ALTER TABLE knowledge_model_package_event ADD CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (package_uuid, uuid); \
        \ALTER TABLE knowledge_model_package_event ADD CONSTRAINT knowledge_model_package_event_package_uuid_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE knowledge_model_package ADD COLUMN public boolean NOT NULL DEFAULT FALSE;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
