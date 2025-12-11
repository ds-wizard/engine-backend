module Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.S3.Common
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.DocumentTemplate.DocumentTemplateS3

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) drop tables"
  let sql =
        "DROP TABLE IF EXISTS document_template_draft_data CASCADE;\
        \DROP TABLE IF EXISTS document_template_asset CASCADE;\
        \DROP TABLE IF EXISTS document_template_file CASCADE;\
        \DROP TABLE IF EXISTS document_template_format_step CASCADE;\
        \DROP TABLE IF EXISTS document_template_format CASCADE;\
        \DROP TABLE IF EXISTS document_template CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropBucket :: AppContextM ()
dropBucket = do
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())

dropFunctions :: AppContextM Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/DocumentTemplate) drop functions"
  let sql = "DROP FUNCTION IF EXISTS create_persistent_command_from_project_file_delete;"
  let action conn = execute_ conn sql
  runDB action

dropTriggers :: AppContextM Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/DocumentTemplate) drop tables"
  let sql = "DROP TRIGGER IF EXISTS trigger_on_after_document_template_asset_delete ON project_file;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM ()
createTables = do
  createTemplateTable
  createTemplateFormatTable
  createTemplateFileTable
  createTemplateAssetTable
  makeBucket
  makeBucketPublicReadOnly

createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) create table"
  let sql =
        "CREATE TABLE document_template \
        \( \
        \    id                varchar          NOT NULL, \
        \    name              varchar          NOT NULL, \
        \    organization_id   varchar          NOT NULL, \
        \    template_id       varchar          NOT NULL, \
        \    version           varchar          NOT NULL, \
        \    metamodel_version sem_ver_2_tuple  NOT NULL, \
        \    description       varchar          NOT NULL, \
        \    readme            varchar          NOT NULL, \
        \    license           varchar          NOT NULL, \
        \    allowed_packages  jsonb            NOT NULL, \
        \    created_at        timestamptz      NOT NULL, \
        \    tenant_uuid       uuid             NOT NULL, \
        \    updated_at        timestamptz      NOT NULL, \
        \    phase             varchar          NOT NULL, \
        \    non_editable      boolean          NOT NULL, \
        \    CONSTRAINT document_template_pk PRIMARY KEY (id, tenant_uuid), \
        \    CONSTRAINT document_template_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX document_template_organization_id_template_id_index ON document_template (organization_id, template_id, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action

createTemplateFormatTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateFormat) create table"
  let sql =
        "CREATE TABLE document_template_format \
        \( \
        \    document_template_id varchar     NOT NULL, \
        \    uuid                 uuid        NOT NULL, \
        \    name                 varchar     NOT NULL, \
        \    icon                 varchar     NOT NULL, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT document_template_format_pk PRIMARY KEY (uuid, document_template_id, tenant_uuid), \
        \    CONSTRAINT document_template_format_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE TABLE document_template_format_step \
        \( \
        \    document_template_id varchar     NOT NULL, \
        \    format_uuid          uuid        NOT NULL, \
        \    position             int         NOT NULL, \
        \    name                 varchar     NOT NULL, \
        \    options              jsonb       NOT NULL, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT document_template_format_step_pk PRIMARY KEY (document_template_id, format_uuid, position, tenant_uuid), \
        \    CONSTRAINT document_template_format_step_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_id, format_uuid, tenant_uuid) REFERENCES document_template_format (document_template_id, uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_step_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTemplateFileTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateFile) create table"
  let sql =
        "CREATE TABLE document_template_file \
        \( \
        \    document_template_id varchar     NOT NULL, \
        \    uuid                 uuid        NOT NULL, \
        \    file_name            varchar     NOT NULL, \
        \    content              varchar     NOT NULL, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT document_template_file_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT document_template_file_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTemplateAssetTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateAsset) create table"
  let sql =
        "CREATE TABLE document_template_asset \
        \( \
        \    document_template_id varchar     NOT NULL, \
        \    uuid                 uuid        NOT NULL, \
        \    file_name            varchar     NOT NULL, \
        \    content_type         varchar     NOT NULL, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    file_size            bigint      NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT document_template_asset_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT document_template_asset_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_asset_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createDraftDataTable :: AppContextM Int64
createDraftDataTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateDraftData) create table"
  let sql =
        "CREATE TABLE document_template_draft_data \
        \( \
        \    document_template_id varchar     NOT NULL, \
        \    project_uuid   uuid, \
        \    format_uuid          uuid, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at                   timestamptz NOT NULL, \
        \    knowledge_model_editor_uuid  uuid, \
        \    CONSTRAINT document_template_draft_data_pk PRIMARY KEY (document_template_id, tenant_uuid), \
        \    CONSTRAINT document_template_draft_data_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_draft_data_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT document_template_draft_data_knowledge_model_editor_uuid_fk FOREIGN KEY (knowledge_model_editor_uuid) REFERENCES knowledge_model_editor (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_draft_data_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: AppContextM Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/DocumentTemplate) create functions"
  createPersistentCommandFromDocumentTemplateAssetDeleteFunction

createPersistentCommandFromDocumentTemplateAssetDeleteFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_document_template_asset_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM create_persistent_command( \
        \            'document_template_asset', \
        \            'deleteFromS3', \
        \            jsonb_build_object('documentTemplateId', OLD.document_template_id, 'assetUuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: AppContextM Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/DocumentTemplate) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_document_template_asset_delete \
        \    AFTER DELETE \
        \    ON document_template_asset \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_document_template_asset_delete();"
  let action conn = execute_ conn sql
  runDB action
