module Wizard.Database.Migration.Development.Document.DocumentSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Document) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Document) ended"

dropTables = do
  dropDocumentQueueTable
  dropDocumentTable

dropDocumentTable = do
  logInfo _CMP_MIGRATION "(Table/Document) drop tables"
  let sql = "drop table if exists document cascade;"
  let action conn = execute_ conn sql
  runDB action

dropDocumentQueueTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentQueue) drop tables"
  let sql = "drop table if exists document_queue;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createDocumentTable
  createDocumentQueueTable

createDocumentTable = do
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
        \     template_id varchar not null, \
        \     format_uuid uuid not null, \
        \     creator_uuid uuid, \
        \     retrieved_at timestamptz, \
        \     finished_at timestamptz, \
        \     created_at timestamptz not null, \
        \     file_name varchar, \
        \     content_type varchar, \
        \     worker_log varchar, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \         constraint document_app_uuid_fk \
        \             references app \
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
        \   add constraint document_template_id_fk \
        \      foreign key (template_id) references template (id); \
        \  \
        \ alter table document \
        \   add constraint document_user_entity_uuid_fk \
        \      foreign key (creator_uuid) references user_entity; \
        \  \
        \ create index document_questionnaire_uuid_index \
        \   on document (questionnaire_uuid);"
  let action conn = execute_ conn sql
  runDB action

createDocumentQueueTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentQueue) create table"
  let sql =
        " create table document_queue \
        \ ( \
        \   id serial not null, \
        \   document_uuid uuid not null \
        \      constraint document_queue_document_uuid_fk \
        \         references document, \
        \   document_context json not null, \
        \   created_by uuid, \
        \   created_at timestamptz not null, \
        \   app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \      constraint branch_app_uuid_fk \
        \         references app \
        \ ); \
        \  \
        \ create unique index document_queue_id_uindex \
        \   on document_queue (id); \
        \  \
        \ alter table document_queue \
        \   add constraint document_queue_pk \
        \      primary key (id); "
  let action conn = execute_ conn sql
  runDB action
