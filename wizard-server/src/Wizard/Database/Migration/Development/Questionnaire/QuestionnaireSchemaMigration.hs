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
        "DROP TABLE IF EXISTS questionnaire_comment CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_comment_thread CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_perm_group CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_perm_user CASCADE; \
        \DROP TABLE IF EXISTS questionnaire CASCADE; "
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createQtnTable
  createQtnAclUserTable
  createQtnAclGroupTable
  createQtnCommentThreadTable
  createQtnCommentTable

createQtnTable = do
  logInfo _CMP_MIGRATION "(Table/Questionnaire) create table"
  let sql =
        "CREATE TABLE questionnaire \
        \( \
        \    uuid                        uuid        NOT NULL, \
        \    name                        varchar     NOT NULL, \
        \    visibility                  varchar     NOT NULL, \
        \    sharing                     varchar     NOT NULL, \
        \    package_id                  varchar     NOT NULL, \
        \    selected_question_tag_uuids json        NOT NULL, \
        \    document_template_id        varchar, \
        \    format_uuid                 uuid, \
        \    created_by                  uuid, \
        \    events                      json        NOT NULL, \
        \    versions                    json        NOT NULL, \
        \    created_at                  timestamptz NOT NULL, \
        \    updated_at                  timestamptz NOT NULL, \
        \    description                 varchar, \
        \    is_template                 boolean     NOT NULL, \
        \    squashed                    boolean     NOT NULL, \
        \    tenant_uuid                 uuid        NOT NULL, \
        \    project_tags                text[]      NOT NULL, \
        \    answered_questions          int         NOT NULL, \
        \    unanswered_questions        int         NOT NULL, \
        \    CONSTRAINT questionnaire_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT questionnaire_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid), \
        \    CONSTRAINT questionnaire_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createQtnAclUserTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireAclUser) create table"
  let sql =
        "CREATE TABLE questionnaire_perm_user \
        \( \
        \    questionnaire_uuid uuid   NOT NULL, \
        \    user_uuid          uuid   NOT NULL, \
        \    perms              text[] NOT NULL, \
        \    tenant_uuid        uuid   NOT NULL, \
        \    CONSTRAINT questionnaire_perm_user_pk PRIMARY KEY (user_uuid, questionnaire_uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_perm_user_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_perm_user_user_uuid_fk FOREIGN KEY (user_uuid, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_perm_user_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createQtnAclGroupTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireAclGroup) create table"
  let sql =
        "CREATE TABLE questionnaire_perm_group \
        \( \
        \    questionnaire_uuid uuid   NOT NULL, \
        \    user_group_uuid    uuid   NOT NULL, \
        \    perms              text[] NOT NULL, \
        \    tenant_uuid        uuid   NOT NULL, \
        \    CONSTRAINT questionnaire_perm_group_pk PRIMARY KEY (user_group_uuid, questionnaire_uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_perm_group_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_perm_group_user_group_uuid_fk FOREIGN KEY (user_group_uuid, tenant_uuid) REFERENCES user_group (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_perm_group_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createQtnCommentThreadTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireCommentThread) create table"
  let sql =
        "CREATE TABLE questionnaire_comment_thread \
        \( \
        \    uuid               uuid        NOT NULL, \
        \    path               text        NOT NULL, \
        \    resolved           bool        NOT NULL, \
        \    private            bool        NOT NULL, \
        \    questionnaire_uuid uuid        NOT NULL, \
        \    created_by         uuid, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_comment_thread_pk PRIMARY KEY (uuid, questionnaire_uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createQtnCommentTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireComment) create table"
  let sql =
        "CREATE TABLE questionnaire_comment \
        \( \
        \    uuid                uuid        NOT NULL, \
        \    text                text        NOT NULL, \
        \    comment_thread_uuid uuid, \
        \    created_by          uuid, \
        \    created_at          timestamptz NOT NULL, \
        \    updated_at          timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_comment_pk PRIMARY KEY (uuid, comment_thread_uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
