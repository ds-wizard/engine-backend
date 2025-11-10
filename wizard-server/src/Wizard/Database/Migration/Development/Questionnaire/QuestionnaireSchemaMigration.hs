module Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.Questionnaire.QuestionnaireFileS3

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Questionnaire) drop tables"
  let sql =
        "DROP TRIGGER IF EXISTS trigger_on_after_questionnaire_file_delete ON questionnaire_file; \
        \DROP FUNCTION IF EXISTS create_persistent_command_from_questionnaire_file_delete; \
        \DROP TABLE IF EXISTS questionnaire_file CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_version CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_comment CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_comment_thread CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_perm_group CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_perm_user CASCADE; \
        \DROP TABLE IF EXISTS questionnaire_event; \
        \DROP TYPE IF EXISTS questionnaire_event_type; \
        \DROP TYPE IF EXISTS value_type; \
        \DROP TABLE IF EXISTS questionnaire CASCADE; "
  let action conn = execute_ conn sql
  runDB action

dropBucket :: AppContextM ()
dropBucket = do
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())

dropTriggers :: AppContextM Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Questionnaire) drop tables"
  let sql = "DROP TRIGGER IF EXISTS trigger_on_after_questionnaire_file_delete ON questionnaire_file;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM ()
createTables = do
  createQtnTable
  createQtnEventTable
  createQtnAclUserTable
  createQtnAclGroupTable
  createQtnCommentThreadTable
  createQtnCommentTable
  createQtnVersionTable
  createQtnFileTable
  createPersistentCommandFromQuestionnaireFileDeleteFunction
  makeBucket

createQtnTable = do
  logInfo _CMP_MIGRATION "(Table/Questionnaire) create table"
  let sql =
        "CREATE TABLE questionnaire \
        \( \
        \    uuid                        uuid        NOT NULL, \
        \    name                        varchar     NOT NULL, \
        \    visibility                  varchar     NOT NULL, \
        \    sharing                     varchar     NOT NULL, \
        \    knowledge_model_package_id  varchar     NOT NULL, \
        \    selected_question_tag_uuids uuid[]      NOT NULL, \
        \    document_template_id        varchar, \
        \    format_uuid                 uuid, \
        \    created_by                  uuid, \
        \    created_at                  timestamptz NOT NULL, \
        \    updated_at                  timestamptz NOT NULL, \
        \    description                 varchar, \
        \    is_template                 boolean     NOT NULL, \
        \    squashed                    boolean     NOT NULL, \
        \    tenant_uuid                 uuid        NOT NULL, \
        \    project_tags                text[]      NOT NULL, \
        \    CONSTRAINT questionnaire_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_knowledge_model_package_id_fk FOREIGN KEY (knowledge_model_package_id, tenant_uuid) REFERENCES knowledge_model_package (id, tenant_uuid), \
        \    CONSTRAINT questionnaire_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid), \
        \    CONSTRAINT questionnaire_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createQtnEventTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireEvent) create table"
  let sql =
        "CREATE TYPE questionnaire_event_type AS ENUM ('ClearReplyEvent', 'SetReplyEvent', 'SetLabelsEvent', 'SetPhaseEvent'); \
        \CREATE TYPE value_type AS ENUM ('IntegrationReply', 'AnswerReply', 'MultiChoiceReply', 'ItemListReply', 'StringReply', 'ItemSelectReply', 'FileReply'); \
        \CREATE TABLE IF NOT EXISTS questionnaire_event \
        \( \
        \    uuid               uuid                     NOT NULL, \
        \    event_type         questionnaire_event_type NOT NULL, \
        \    path               text, \
        \    created_at         timestamptz              NOT NULL, \
        \    created_by         uuid, \
        \    questionnaire_uuid uuid                     NOT NULL, \
        \    tenant_uuid        uuid                     NOT NULL, \
        \    value_type         value_type, \
        \    value              text[], \
        \    value_id           text, \
        \    value_raw          jsonb, \
        \    CONSTRAINT questionnaire_event_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_event_created_by_fk FOREIGN KEY (created_by, tenant_uuid) references user_entity(uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_event_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) references questionnaire(uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) references tenant(uuid) \
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
        \    uuid                   uuid        NOT NULL, \
        \    path                   text        NOT NULL, \
        \    resolved               bool        NOT NULL, \
        \    private                bool        NOT NULL, \
        \    questionnaire_uuid     uuid        NOT NULL, \
        \    created_by             uuid, \
        \    created_at             timestamptz NOT NULL, \
        \    updated_at             timestamptz NOT NULL, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    assigned_to            uuid, \
        \    assigned_by            uuid, \
        \    notification_required  bool        NOT NULL DEFAULT false, \
        \    CONSTRAINT questionnaire_comment_thread_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_comment_thread_questionnaire_uuid FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_comment_thread_assigned_to FOREIGN KEY (assigned_to, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_comment_thread_assigned_by FOREIGN KEY (assigned_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_comment_thread_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
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
        \    tenant_uuid         uuid        NOT NULL, \
        \    CONSTRAINT questionnaire_comment_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_comment_comment_thread_uuid FOREIGN KEY (comment_thread_uuid, tenant_uuid) REFERENCES questionnaire_comment_thread (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_comment_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createQtnVersionTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireVersion) create table"
  let sql =
        "CREATE TABLE questionnaire_version \
        \( \
        \    uuid               uuid        NOT NULL, \
        \    name               varchar     NOT NULL, \
        \    description        varchar, \
        \    event_uuid         uuid        NOT NULL, \
        \    questionnaire_uuid uuid        NOT NULL, \
        \    tenant_uuid        uuid        NOT NULL, \
        \    created_by         uuid, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_version_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_version_event_uuid_fk FOREIGN KEY (event_uuid, tenant_uuid) REFERENCES questionnaire_event (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT questionnaire_version_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT questionnaire_version_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT questionnaire_version_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid) ON DELETE SET NULL \
        \);"
  let action conn = execute_ conn sql
  runDB action

createQtnFileTable = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireFile) create table"
  let sql =
        "CREATE TABLE questionnaire_file \
        \( \
        \    uuid               uuid        NOT NULL, \
        \    file_name          varchar     NOT NULL, \
        \    content_type       varchar     NOT NULL, \
        \    file_size          bigint      NOT NULL, \
        \    questionnaire_uuid uuid        NOT NULL, \
        \    created_by         uuid, \
        \    tenant_uuid        uuid        NOT NULL, \
        \    created_at         timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_file_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_file_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_file_user_uuid_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT questionnaire_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createPersistentCommandFromQuestionnaireFileDeleteFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_questionnaire_file_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM create_persistent_command( \
        \            'questionnaire_file', \
        \            'deleteFromS3', \
        \            jsonb_build_object('questionnaireUuid', OLD.questionnaire_uuid, 'fileUuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: AppContextM Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/QuestionnaireFile) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_questionnaire_file_delete \
        \    AFTER DELETE \
        \    ON questionnaire_file \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_questionnaire_file_delete();"
  let action conn = execute_ conn sql
  runDB action
