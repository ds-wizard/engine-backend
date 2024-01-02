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
  dropTemplateAssetTable
  dropTemplateFileTable
  dropTemplateTable
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())

dropTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) drop tables"
  let sql = "DROP TABLE IF EXISTS document_template CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropTemplateFileTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateFile) drop tables"
  let sql = "DROP TABLE IF EXISTS document_template_file CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropTemplateAssetTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateAsset) drop tables"
  let sql = "DROP TABLE IF EXISTS document_template_asset CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM ()
createTables = do
  createTemplateTable
  createTemplateFileTable
  createTemplateAssetTable
  makeBucket

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
        \    CONSTRAINT document_template_pk PRIMARY KEY (id) \
        \); \
        \ \
        \CREATE INDEX document_template_organization_id_template_id_index ON document_template (organization_id, template_id);"
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
        \    CONSTRAINT document_template_file_document_template_id_fk FOREIGN KEY (document_template_id) REFERENCES document_template (id) \
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
        \    CONSTRAINT document_template_asset_document_template_id_fk FOREIGN KEY (document_template_id) REFERENCES document_template (id) \
        \); \
        \ \
        \CREATE UNIQUE INDEX document_template_asset_uuid_uindex ON document_template_asset (uuid);"
  let action conn = execute_ conn sql
  runDB action
