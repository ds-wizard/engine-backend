module Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Migration/Questionnaire) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Migration/Questionnaire) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/Questionnaire) drop tables"
  let sql = "drop table if exists qtn_migration;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/Questionnaire) create table"
  let sql =
        " create table qtn_migration \
        \ ( \
        \   old_questionnaire_uuid uuid not null, \
        \   new_questionnaire_uuid uuid not null, \
        \   resolved_question_uuids json not null, \
        \   constraint qtn_migration_pk \
        \      primary key (old_questionnaire_uuid, new_questionnaire_uuid) \
        \ ); \
        \  \
        \ alter table qtn_migration \
        \   add constraint qtn_migration_old_questionnaire_uuid_fk \
        \      foreign key (old_questionnaire_uuid) references questionnaire; \
        \  \
        \ alter table qtn_migration \
        \   add constraint qtn_migration_new_questionnaire_uuid_fk \
        \      foreign key (new_questionnaire_uuid) references questionnaire; "
  let action conn = execute_ conn sql
  runDB action
