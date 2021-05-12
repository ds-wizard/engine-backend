module Registry.Database.Migration.Development.Template.TemplateSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.Template.TemplateS3
import Registry.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Template) started"
  dropTables
  createTables
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())
  makeBucket
  logInfo _CMP_MIGRATION "(Table/Template) ended"

dropTables = do
  dropTemplateAssetTable
  dropTemplateFileTable
  dropTemplateTable

dropTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/Template) drop tables"
  let sql = "drop table if exists template;"
  let action conn = execute_ conn sql
  runDB action

dropTemplateFileTable = do
  logInfo _CMP_MIGRATION "(Table/TemplateFile) drop tables"
  let sql = "drop table if exists template_file;"
  let action conn = execute_ conn sql
  runDB action

dropTemplateAssetTable = do
  logInfo _CMP_MIGRATION "(Table/TemplateAsset) drop tables"
  let sql = "drop table if exists template_asset;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createTemplateTable
  createTemplateFileTable
  createTemplateAssetTable

createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/Template) create table"
  let sql =
        "create table template \
          \ ( \
          \     id                     varchar                  not null \
          \         constraint template_pk \
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
          \     recommended_package_id varchar, \
          \     formats                json                     not null, \
          \     created_at             timestamp with time zone not null \
          \ ); \
          \create unique index template_id_uindex \
          \     on template (id); \
          \create index template_organization_id_template_id_index \
          \     on template (organization_id, template_id); "
  let action conn = execute_ conn sql
  runDB action

createTemplateFileTable = do
  logInfo _CMP_MIGRATION "(Table/TemplateFile) create table"
  let sql =
        " create table template_file \
        \ ( \
        \   template_id varchar not null, \
        \   uuid uuid not null, \
        \   file_name varchar not null, \
        \   content varchar not null \
        \ ); \
        \  \
        \ alter table template_file \
        \   add constraint template_file_template_id_fk \
        \      foreign key (template_id) references template (id); \
        \  \
        \ create unique index template_file_uuid_uindex \
        \   on template_file (uuid); \
        \  \
        \ alter table template_file \
        \   add constraint template_file_pk \
        \      primary key (uuid); "
  let action conn = execute_ conn sql
  runDB action

createTemplateAssetTable = do
  logInfo _CMP_MIGRATION "(Table/TemplateAsset) create table"
  let sql =
        " create table template_asset \
        \ ( \
        \   template_id varchar not null, \
        \   uuid uuid not null, \
        \   file_name varchar not null, \
        \   content_type varchar not null \
        \ ); \
        \  \
        \ alter table template_asset \
        \   add constraint template_asset_template_id_fk \
        \      foreign key (template_id) references template (id); \
        \  \
        \ create unique index template_asset_uuid_uindex \
        \   on template_asset (uuid); \
        \  \
        \ alter table template_asset \
        \   add constraint template_asset_pk \
        \      primary key (uuid); "
  let action conn = execute_ conn sql
  runDB action
