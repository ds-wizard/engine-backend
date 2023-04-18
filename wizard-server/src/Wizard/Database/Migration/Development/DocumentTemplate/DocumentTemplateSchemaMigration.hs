module Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple

import Shared.Common.S3.Common
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) started"
  dropFunctions
  dropTables
  createTables
  createFunctions
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) ended"

dropTables = do
  dropDraftDataTable
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

dropDraftDataTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateDraftData) drop tables"
  let sql = "drop table if exists document_template_draft_data cascade;"
  let action conn = execute_ conn sql
  runDB action

dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/DocumentTemplate) drop functions"
  let sql = "DROP FUNCTION IF EXISTS get_template_state;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createTemplateTable
  createTemplateFileTable
  createTemplateAssetTable
  createDraftDataTable
  makeBucket
  makeBucketPublicReadOnly

createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) create table"
  let sql =
        "create table document_template \
        \ ( \
        \     id                     varchar                  not null, \
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
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint document_template_app_uuid_fk \
        \         references app, \
        \     updated_at             timestamp with time zone not null, \
        \     phase                  varchar                  not null default 'ReleasedDocumentTemplatePhase' \
        \ ); \
        \ \
        \alter table document_template \
        \    add constraint document_template_pk primary key (id, app_uuid); \
        \create unique index document_template_id_uindex \
        \     on document_template (id, app_uuid); \
        \ \
        \create index document_template_organization_id_template_id_index \
        \     on document_template (organization_id, template_id, app_uuid); "
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
        \   app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \     constraint document_template_file_app_uuid_fk \
        \       references app, \
        \     created_at             timestamp with time zone not null, \
        \     updated_at             timestamp with time zone not null \
        \ ); \
        \  \
        \ alter table document_template_file \
        \   add constraint document_template_file_template_id_fk \
        \      foreign key (document_template_id, app_uuid) references document_template (id, app_uuid); \
        \  \
        \ create unique index document_template_file_uuid_uindex \
        \   on document_template_file (uuid, app_uuid); \
        \  \
        \ alter table document_template_file \
        \   add constraint document_template_file_pk \
        \      primary key (uuid, app_uuid); "
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
        \   app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \     constraint document_template_asset_app_uuid_fk \
        \       references app, \
        \   file_size bigint not null default 0, \
        \   created_at             timestamp with time zone not null, \
        \   updated_at             timestamp with time zone not null \
        \ ); \
        \  \
        \ alter table document_template_asset \
        \   add constraint document_template_asset_template_id_fk \
        \      foreign key (document_template_id, app_uuid) references document_template (id, app_uuid); \
        \  \
        \ create unique index document_template_asset_uuid_uindex \
        \   on document_template_asset (uuid, app_uuid); \
        \  \
        \ alter table document_template_asset \
        \   add constraint document_template_asset_pk \
        \      primary key (uuid, app_uuid); "
  let action conn = execute_ conn sql
  runDB action

createDraftDataTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateDraftData) create table"
  let sql =
        " create table document_template_draft_data \
        \ ( \
        \   document_template_id varchar not null, \
        \   questionnaire_uuid uuid, \
        \   format_uuid uuid, \
        \   app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \     constraint document_template_draft_data_app_uuid_fk \
        \       references app, \
        \   created_at             timestamp with time zone not null, \
        \   updated_at             timestamp with time zone not null \
        \ ); \
        \  \
        \ alter table document_template_draft_data \
        \   add constraint document_template_draft_data_document_template_id_fk \
        \      foreign key (document_template_id, app_uuid) references document_template (id, app_uuid); \
        \  \
        \ create unique index document_template_draft_data_document_template_id_uindex \
        \   on document_template_draft_data (document_template_id, app_uuid); \
        \  \
        \ alter table document_template_draft_data \
        \   add constraint document_template_draft_data_pk \
        \      primary key (document_template_id, app_uuid); "
  let action conn = execute_ conn sql
  runDB action

createFunctions = do
  logInfo _CMP_MIGRATION "(Function/DocumentTemplate) create functions"
  createGetTemplateStateFn

createGetTemplateStateFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_template_state(remote_version varchar, local_version varchar, actual_metamodel_version int, template_metamodel_version int) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN actual_metamodel_version != template_metamodel_version IS NULL THEN 'UnsupportedMetamodelVersionDocumentTemplateState' \
        \               WHEN remote_version IS NULL THEN 'UnknownDocumentTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'LT' THEN 'UnpublishedDocumentTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'EQ' THEN 'UpToDateDocumentTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'GT' THEN 'OutdatedDocumentTemplateState' \
        \               END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
