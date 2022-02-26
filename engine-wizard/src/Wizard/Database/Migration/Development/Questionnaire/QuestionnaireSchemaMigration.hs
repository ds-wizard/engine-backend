module Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Questionnaire) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Questionnaire) ended"

dropTables = do
  dropQtnPermTable
  dropQtnTable

dropQtnTable = do
  logInfo _CMP_MIGRATION "(Table/Questionnaire) drop tables"
  let sql = "drop table if exists questionnaire cascade;"
  let action conn = execute_ conn sql
  runDB action

dropQtnPermTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireAcl) drop tables"
  let sql =
        "drop table if exists questionnaire_acl_user cascade; \
            \drop table if exists questionnaire_acl_group cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createQtnTable
  createQtnPermTable

createQtnTable = do
  logInfo _CMP_MIGRATION "(Table/Questionnaire) create table"
  let sql =
        " create table questionnaire \
        \ ( \
        \     uuid uuid not null, \
        \     name varchar not null, \
        \     visibility varchar not null, \
        \     sharing varchar not null, \
        \     package_id varchar not null, \
        \     selected_question_tag_uuids json not null, \
        \     template_id varchar, \
        \     format_uuid uuid, \
        \     creator_uuid uuid, \
        \     events json not null, \
        \     versions json not null, \
        \     created_at timestamptz not null, \
        \     updated_at timestamptz not null, \
        \     description varchar, \
        \     is_template boolean not null, \
        \     squashed boolean not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \         constraint questionnaire_app_uuid_fk \
        \             references app, \
        \     project_tags text[] not null default '{}' \
        \ ); \
        \  \
        \ create unique index questionnaire_uuid_uindex \
        \     on questionnaire (uuid); \
        \  \
        \ alter table questionnaire \
        \     add constraint questionnaire_pk \
        \         primary key (uuid); \
        \  \
        \ alter table questionnaire \
        \   add constraint questionnaire_package_id_fk \
        \      foreign key (package_id, app_uuid) references package (id, app_uuid); \
        \ alter table questionnaire \
        \   add constraint questionnaire_template_id_fk \
        \      foreign key (template_id, app_uuid) references template (id, app_uuid); \
        \ alter table questionnaire \
        \   add constraint questionnaire_user_entity_uuid_fk \
        \      foreign key (creator_uuid) references user_entity;"
  let action conn = execute_ conn sql
  runDB action

createQtnPermTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireAcl) create table"
  let sql =
        "create table questionnaire_acl_user \
         \ ( \
         \     uuid               uuid   not null \
         \         constraint questionnaire_user_acl_pk \
         \             primary key, \
         \     user_uuid          uuid   not null \
         \         constraint questionnaire_acl_user_user_uuid_fk \
         \             references user_entity on delete cascade, \
         \     perms              text[] not null, \
         \     questionnaire_uuid uuid   not null \
         \         constraint questionnaire_acl_user_questionnaire_uuid_fk \
         \             references questionnaire on delete cascade on update cascade \
         \ ); \
         \  \
         \ create unique index questionnaire_acl_user_uuid_uindex \
         \     on questionnaire_acl_user (uuid); \
         \  \
         \ create table questionnaire_acl_group \
         \ ( \
         \     uuid               uuid    not null \
         \         constraint questionnaire_acl_group_pk \
         \             primary key, \
         \     group_id           varchar not null \
         \         constraint questionnaire_acl_group_group_id_fk \
         \             references acl_group on delete cascade, \
         \     perms              text[]  not null, \
         \     questionnaire_uuid uuid    not null \
         \         constraint questionnaire_acl_group_questionnaire_uuid_fk \
         \             references questionnaire on delete cascade \
         \ ); \
         \  \
         \ create unique index questionnaire_acl_group_uuid_uindex \
         \     on questionnaire_acl_group (uuid); "
  let action conn = execute_ conn sql
  runDB action
