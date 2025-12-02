module Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Common.Util.Logger

dropTables :: AppContextM ()
dropTables = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) drop tables"
  let sql =
        "DROP TABLE IF EXISTS document_template_asset CASCADE;\
        \DROP TABLE IF EXISTS document_template_file CASCADE;\
        \DROP TABLE IF EXISTS document_template_format_step CASCADE;\
        \DROP TABLE IF EXISTS document_template_format CASCADE;\
        \DROP TABLE IF EXISTS document_template CASCADE;"
  let action conn = execute_ conn sql
  runDB action
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())

createTables :: AppContextM ()
createTables = do
  createTemplateTable
  createTemplateFormatTable
  createTemplateFileTable
  createTemplateAssetTable
  makeBucket

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
        \    CONSTRAINT document_template_pk PRIMARY KEY (id) \
        \); \
        \ \
        \CREATE INDEX document_template_organization_id_template_id_index ON document_template (organization_id, template_id);"
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
        \    CONSTRAINT document_template_format_pk PRIMARY KEY (document_template_id, uuid), \
        \    CONSTRAINT document_template_format_document_template_id_fk FOREIGN KEY (document_template_id) REFERENCES document_template (id) ON DELETE CASCADE \
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
        \    CONSTRAINT document_template_format_step_pk PRIMARY KEY (document_template_id, format_uuid, position), \
        \    CONSTRAINT document_template_format_step_document_template_id_fk FOREIGN KEY (document_template_id) REFERENCES document_template (id) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_id, format_uuid) REFERENCES document_template_format (document_template_id, uuid) ON DELETE CASCADE \
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
        \    CONSTRAINT document_template_file_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT document_template_file_document_template_id_fk FOREIGN KEY (document_template_id) REFERENCES document_template (id) ON DELETE CASCADE \
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
        \    CONSTRAINT document_template_asset_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT document_template_asset_document_template_id_fk FOREIGN KEY (document_template_id) REFERENCES document_template (id) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE UNIQUE INDEX document_template_asset_uuid_uindex ON document_template_asset (uuid);"
  let action conn = execute_ conn sql
  runDB action
