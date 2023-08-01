module Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Common.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) ended"

dropTables = do
  dropTemplateAssetTable
  dropTemplateFileTable
  dropTemplateTable
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())

dropTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) drop tables"
  let sql = "drop table if exists document_template cascade;"
  let action conn = execute_ conn sql
  runDB action

dropTemplateFileTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateFile) drop tables"
  let sql = "drop table if exists document_template_file cascade;"
  let action conn = execute_ conn sql
  runDB action

dropTemplateAssetTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateAsset) drop tables"
  let sql = "drop table if exists document_template_asset cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createTemplateTable
  createTemplateFileTable
  createTemplateAssetTable
  makeBucket

createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) create table"
  let sql =
        "create table document_template \
        \ ( \
        \     id                     varchar                  not null \
        \         constraint document_template_pk \
        \             primary key, \
        \     name                   varchar                  not null, \
        \     organization_id        varchar                  not null, \
        \     template_id            varchar                  not null, \
        \     version                varchar                  not null, \
        \     metamodel_version      integer                  not null, \
        \     description            varchar                  not null, \
        \     readme                 varchar                  not null, \
        \     license                varchar                  not null, \
        \     allowed_packages       json                     not null, \
        \     formats                json                     not null, \
        \     created_at             timestamp with time zone not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null, \
        \     updated_at             timestamp with time zone not null, \
        \     phase                  varchar                  not null default 'ReleasedDocumentTemplatePhase' \
        \ ); \
        \create unique index document_template_id_uindex \
        \     on document_template (id); \
        \create index document_template_organization_id_template_id_index \
        \     on document_template (organization_id, template_id); "
  let action conn = execute_ conn sql
  runDB action

createTemplateFileTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateFile) create table"
  let sql =
        " create table document_template_file \
        \ ( \
        \   document_template_id varchar not null, \
        \   uuid uuid not null, \
        \   file_name varchar not null, \
        \   content varchar not null, \
        \   app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null, \
        \   created_at             timestamp with time zone not null, \
        \   updated_at             timestamp with time zone not null \
        \ ); \
        \  \
        \ alter table document_template_file \
        \   add constraint document_template_file_template_id_fk \
        \      foreign key (document_template_id) references document_template (id); \
        \  \
        \ create unique index document_template_file_uuid_uindex \
        \   on document_template_file (uuid); \
        \  \
        \ alter table document_template_file \
        \   add constraint document_template_file_pk \
        \      primary key (uuid); "
  let action conn = execute_ conn sql
  runDB action

createTemplateAssetTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateAsset) create table"
  let sql =
        " create table document_template_asset \
        \ ( \
        \   document_template_id varchar not null, \
        \   uuid uuid not null, \
        \   file_name varchar not null, \
        \   content_type varchar not null, \
        \   app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null, \
        \   file_size bigint not null default 0, \
        \   created_at             timestamp with time zone not null, \
        \   updated_at             timestamp with time zone not null \
        \ ); \
        \  \
        \ alter table document_template_asset \
        \   add constraint document_template_asset_template_id_fk \
        \      foreign key (document_template_id) references document_template (id); \
        \  \
        \ create unique index document_template_asset_uuid_uindex \
        \   on document_template_asset (uuid); \
        \  \
        \ alter table document_template_asset \
        \   add constraint document_template_asset_pk \
        \      primary key (uuid); "
  let action conn = execute_ conn sql
  runDB action
