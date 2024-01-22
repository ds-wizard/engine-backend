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
        \DROP TABLE IF EXISTS document_template CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropBucket :: AppContextM ()
dropBucket = do
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())

createTables :: AppContextM ()
createTables = do
  createTemplateTable
  createTemplateFileTable
  createTemplateAssetTable
  makeBucket
  makeBucketPublicReadOnly

createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) create table"
  let sql =
        "CREATE TABLE document_template \
        \( \
        \    id                varchar     NOT NULL, \
        \    name              varchar     NOT NULL, \
        \    organization_id   varchar     NOT NULL, \
        \    template_id       varchar     NOT NULL, \
        \    version           varchar     NOT NULL, \
        \    metamodel_version integer     NOT NULL, \
        \    description       varchar     NOT NULL, \
        \    readme            varchar     NOT NULL, \
        \    license           varchar     NOT NULL, \
        \    allowed_packages  json        NOT NULL, \
        \    formats           json        NOT NULL, \
        \    created_at        timestamptz NOT NULL, \
        \    tenant_uuid       uuid        NOT NULL, \
        \    updated_at        timestamptz NOT NULL, \
        \    phase             varchar     NOT NULL, \
        \    non_editable      boolean     NOT NULL, \
        \    CONSTRAINT document_template_pk PRIMARY KEY (id, tenant_uuid), \
        \    CONSTRAINT document_template_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \); \
        \ \
        \CREATE INDEX document_template_organization_id_template_id_index ON document_template (organization_id, template_id, tenant_uuid);"
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
        \    CONSTRAINT document_template_file_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid), \
        \    CONSTRAINT document_template_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
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
        \    CONSTRAINT document_template_asset_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid), \
        \    CONSTRAINT document_template_asset_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
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
        \    questionnaire_uuid   uuid, \
        \    format_uuid          uuid, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    CONSTRAINT document_template_draft_data_pk PRIMARY KEY (document_template_id, tenant_uuid), \
        \    CONSTRAINT document_template_draft_data_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT document_template_draft_data_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
