module Wizard.Database.Migration.Development.Document.DocumentSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Document) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Document) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Document) drop tables"
  let sql = "drop table if exists document cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Document) create table"
  let sql =
        " create table document \
        \ ( \
        \     uuid uuid not null, \
        \     name varchar not null, \
        \     state varchar not null, \
        \     durability varchar not null, \
        \     questionnaire_uuid uuid not null, \
        \     questionnaire_event_uuid uuid, \
        \     questionnaire_replies_hash bigint not null, \
        \     document_template_id varchar not null, \
        \     format_uuid uuid not null, \
        \     created_by uuid, \
        \     retrieved_at timestamptz, \
        \     finished_at timestamptz, \
        \     created_at timestamptz not null, \
        \     file_name varchar, \
        \     content_type varchar, \
        \     worker_log varchar, \
        \     tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \         constraint document_tenant_uuid_fk \
        \             references tenant, \
        \     file_size bigint \
        \ ); \
        \  \
        \ create unique index document_uuid_uindex \
        \     on document (uuid); \
        \  \
        \ alter table document \
        \     add constraint document_pk \
        \         primary key (uuid); \
        \ alter table document \
        \   add constraint document_questionnaire_uuid_fk \
        \      foreign key (questionnaire_uuid) references questionnaire; \
        \  \
        \ alter table document \
        \   add constraint document_document_template_id_fk \
        \      foreign key (document_template_id, tenant_uuid) references document_template (id, tenant_uuid); \
        \ alter table document \
        \   add constraint document_user_entity_uuid_fk \
        \      foreign key (created_by) references user_entity; \
        \  \
        \ create index document_questionnaire_uuid_index \
        \   on document (questionnaire_uuid);"
  let action conn = execute_ conn sql
  runDB action
