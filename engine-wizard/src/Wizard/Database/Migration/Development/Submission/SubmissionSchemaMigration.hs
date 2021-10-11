module Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Submission) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Submission) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) drop tables"
  let sql = "drop table if exists submission cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) create table"
  let sql =
        "create table submission \
        \ ( \
        \     uuid uuid not null, \
        \     state varchar not null, \
        \     location varchar, \
        \     returned_data varchar, \
        \     service_id varchar not null, \
        \     document_uuid uuid, \
        \     created_by uuid, \
        \     created_at timestamptz, \
        \     updated_at timestamptz not null \
        \ ); \
        \  \
        \ create unique index submission_uuid_uindex \
        \     on submission (uuid); \
        \  \
        \ alter table submission \
        \     add constraint submission_pk \
        \         primary key (uuid); \
        \ alter table submission \
        \   add constraint submission_document_uuid_fk \
        \      foreign key (document_uuid) references document (uuid); \
        \  \
        \ alter table submission \
        \   add constraint submission_created_by_fk \
        \      foreign key (created_by) references user_entity (uuid); \
        \  \
        \ create index submission_document_uuid_index \
        \   on submission (document_uuid);"
  let action conn = execute_ conn sql
  runDB action
