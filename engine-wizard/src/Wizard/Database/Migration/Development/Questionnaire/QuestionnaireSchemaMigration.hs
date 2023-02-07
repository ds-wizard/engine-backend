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
  logInfo _CMP_MIGRATION "(Table/Questionnaire) drop tables"
  let sql =
        "drop table if exists questionnaire_comment cascade; \
        \drop table if exists questionnaire_comment_thread cascade; \
        \drop table if exists questionnaire_acl_user cascade; \
        \drop table if exists questionnaire_acl_group cascade; \
        \drop table if exists questionnaire cascade; "
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createQtnTable
  createQtnPermTable
  createQtnCommentTable

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
        \     document_template_id varchar, \
        \     format_uuid uuid, \
        \     created_by uuid, \
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
        \     project_tags text[] not null default '{}', \
        \     answered_questions int not null default 0, \
        \     unanswered_questions int not null default 0 \
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
        \   add constraint questionnaire_document_template_id_fk \
        \      foreign key (document_template_id, app_uuid) references document_template (id, app_uuid); \
        \ alter table questionnaire \
        \   add constraint questionnaire_user_entity_uuid_fk \
        \      foreign key (created_by) references user_entity;"
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

createQtnCommentTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireComment) create table"
  let sql =
        "create table questionnaire_comment_thread \
        \( \
        \    uuid       uuid        not null \
        \        constraint questionnaire_comment_thread_pk \
        \            primary key, \
        \    path       text        not null, \
        \    resolved   bool        not null, \
        \    private    bool        not null, \
        \    questionnaire_uuid uuid   not null \
        \        constraint questionnaire_comment_thread_questionnaire_uuid_fk \
        \            references questionnaire, \
        \    created_by uuid \
        \        constraint questionnaire_comment_thread_user_entity_uuid_fk \
        \            references user_entity, \
        \    created_at timestamptz not null, \
        \    updated_at timestamptz not null \
        \); \
        \ \
        \create unique index questionnaire_comment_thread_uuid_uindex \
        \    on questionnaire_comment_thread (uuid); \
        \ \
        \create table questionnaire_comment \
        \( \
        \    uuid       uuid        not null \
        \        constraint questionnaire_comment_pk \
        \            primary key, \
        \    text   text        not null, \
        \    comment_thread_uuid uuid \
        \        constraint questionnaire_comment_questionnaire_comment_thread_uuid_fk \
        \            references questionnaire_comment_thread, \
        \    created_by uuid \
        \        constraint questionnaire_comment_user_entity_uuid_fk \
        \            references user_entity, \
        \    created_at timestamptz not null, \
        \    updated_at timestamptz not null \
        \); \
        \ \
        \create unique index questionnaire_comment_uuid_uindex \
        \    on questionnaire_comment (uuid); "
  let action conn = execute_ conn sql
  runDB action
