module Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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
        \drop table if exists questionnaire_perm_group cascade; \
        \drop table if exists questionnaire_perm_user cascade; \
        \drop table if exists questionnaire cascade; "
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createQtnTable
  createQtnAclUserTable
  createQtnAclGroupTable
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
        \     tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \         constraint questionnaire_tenant_uuid_fk \
        \             references tenant, \
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
        \      foreign key (package_id, tenant_uuid) references package (id, tenant_uuid); \
        \ alter table questionnaire \
        \   add constraint questionnaire_document_template_id_fk \
        \      foreign key (document_template_id, tenant_uuid) references document_template (id, tenant_uuid); \
        \ alter table questionnaire \
        \   add constraint questionnaire_user_entity_uuid_fk \
        \      foreign key (created_by) references user_entity;"
  let action conn = execute_ conn sql
  runDB action

createQtnAclUserTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireAclUser) create table"
  let sql =
        "create table questionnaire_perm_user \
        \ ( \
        \     questionnaire_uuid uuid   not null \
        \         constraint questionnaire_perm_user_questionnaire_uuid_fk \
        \             references questionnaire on delete cascade, \
        \     user_uuid          uuid   not null \
        \         constraint questionnaire_perm_user_user_uuid_fk \
        \             references user_entity, \
        \     perms              text[] not null, \
        \     tenant_uuid        uuid   not null \
        \         constraint questionnaire_perm_user_tenant_uuid_fk \
        \             references tenant, \
        \     constraint questionnaire_perm_user_pk primary key (user_uuid, questionnaire_uuid, tenant_uuid) \
        \ );"
  let action conn = execute_ conn sql
  runDB action

createQtnAclGroupTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireAclGroup) create table"
  let sql =
        "create table questionnaire_perm_group \
        \ ( \
        \     questionnaire_uuid uuid   not null \
        \         constraint questionnaire_perm_group_questionnaire_uuid_fk \
        \             references questionnaire on delete cascade, \
        \     user_group_uuid          uuid   not null, \
        \     perms              text[] not null, \
        \     tenant_uuid        uuid   not null \
        \         constraint questionnaire_perm_group_tenant_uuid_fk \
        \             references tenant, \
        \     constraint questionnaire_perm_group_pk primary key (user_group_uuid, questionnaire_uuid, tenant_uuid), \
        \     constraint questionnaire_perm_group_user_uuid_tenant_uuid_fk foreign key (user_group_uuid, tenant_uuid) references user_group \
        \ );"
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
