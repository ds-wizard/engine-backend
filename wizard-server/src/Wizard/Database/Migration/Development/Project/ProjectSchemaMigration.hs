module Wizard.Database.Migration.Development.Project.ProjectSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.Project.ProjectFileS3

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Project) drop tables"
  let sql =
        "DROP TABLE IF EXISTS project_file CASCADE; \
        \DROP TABLE IF EXISTS project_version CASCADE; \
        \DROP TABLE IF EXISTS project_comment CASCADE; \
        \DROP TABLE IF EXISTS project_comment_thread CASCADE; \
        \DROP TABLE IF EXISTS project_perm_group CASCADE; \
        \DROP TABLE IF EXISTS project_perm_user CASCADE; \
        \DROP TABLE IF EXISTS project_event; \
        \DROP TYPE IF EXISTS project_event_type; \
        \DROP TYPE IF EXISTS value_type; \
        \DROP TABLE IF EXISTS project CASCADE; "
  let action conn = execute_ conn sql
  runDB action

dropBucket :: AppContextM ()
dropBucket = do
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())

dropFunctions :: AppContextM Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Project) drop functions"
  let sql = "DROP FUNCTION IF EXISTS create_persistent_command_from_project_file_delete;"
  let action conn = execute_ conn sql
  runDB action

dropTriggers :: AppContextM Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Project) drop tables"
  let sql = "DROP TRIGGER IF EXISTS trigger_on_after_project_file_delete ON project_file;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM ()
createTables = do
  createProjectTable
  createProjectEventTable
  createProjectPermUserTable
  createProjectPermGroupTable
  createProjectCommentThreadTable
  createProjectCommentTable
  createProjectVersionTable
  createProjectFileTable
  createPersistentCommandFromProjectFileDeleteFunction
  makeBucket

createProjectTable = do
  logInfo _CMP_MIGRATION "(Table/Project) create table"
  let sql =
        "CREATE TABLE project \
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
        \    CONSTRAINT project_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT project_knowledge_model_package_id_fk FOREIGN KEY (knowledge_model_package_id, tenant_uuid) REFERENCES knowledge_model_package (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT project_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createProjectEventTable = do
  logInfo _CMP_MIGRATION "(Table/ProjectEvent) create table"
  let sql =
        "CREATE TYPE project_event_type AS ENUM ('ClearReplyEvent', 'SetReplyEvent', 'SetLabelsEvent', 'SetPhaseEvent'); \
        \CREATE TYPE value_type AS ENUM ('IntegrationReply', 'AnswerReply', 'MultiChoiceReply', 'ItemListReply', 'StringReply', 'ItemSelectReply', 'FileReply'); \
        \CREATE TABLE IF NOT EXISTS project_event \
        \( \
        \    uuid               uuid                     NOT NULL, \
        \    event_type         project_event_type NOT NULL, \
        \    path               text, \
        \    created_at         timestamptz              NOT NULL, \
        \    created_by         uuid, \
        \    project_uuid uuid                     NOT NULL, \
        \    tenant_uuid        uuid                     NOT NULL, \
        \    value_type         value_type, \
        \    value              text[], \
        \    value_id           text, \
        \    value_raw          jsonb, \
        \    CONSTRAINT project_event_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT project_event_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE SET NULL, \
        \    CONSTRAINT project_event_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createProjectPermUserTable = do
  logInfo _CMP_MIGRATION "(Table/ProjectPermUser) create table"
  let sql =
        "CREATE TABLE project_perm_user \
        \( \
        \    project_uuid uuid   NOT NULL, \
        \    user_uuid          uuid   NOT NULL, \
        \    perms              text[] NOT NULL, \
        \    tenant_uuid        uuid   NOT NULL, \
        \    CONSTRAINT project_perm_user_pk PRIMARY KEY (user_uuid, project_uuid), \
        \    CONSTRAINT project_perm_user_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_perm_user_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_perm_user_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createProjectPermGroupTable = do
  logInfo _CMP_MIGRATION "(Table/ProjectPermGroup) create table"
  let sql =
        "CREATE TABLE project_perm_group \
        \( \
        \    project_uuid uuid   NOT NULL, \
        \    user_group_uuid    uuid   NOT NULL, \
        \    perms              text[] NOT NULL, \
        \    tenant_uuid        uuid   NOT NULL, \
        \    CONSTRAINT project_perm_group_pk PRIMARY KEY (user_group_uuid, project_uuid), \
        \    CONSTRAINT project_perm_group_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_perm_group_user_group_uuid_fk FOREIGN KEY (user_group_uuid) REFERENCES user_group (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_perm_group_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createProjectCommentThreadTable = do
  logInfo _CMP_MIGRATION "(Table/ProjectCommentThread) create table"
  let sql =
        "CREATE TABLE project_comment_thread \
        \( \
        \    uuid                   uuid        NOT NULL, \
        \    path                   text        NOT NULL, \
        \    resolved               bool        NOT NULL, \
        \    private                bool        NOT NULL, \
        \    project_uuid     uuid        NOT NULL, \
        \    created_by             uuid, \
        \    created_at             timestamptz NOT NULL, \
        \    updated_at             timestamptz NOT NULL, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    assigned_to            uuid, \
        \    assigned_by            uuid, \
        \    notification_required  bool        NOT NULL DEFAULT false, \
        \    CONSTRAINT project_comment_thread_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT project_comment_thread_project_uuid FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_comment_thread_assigned_to FOREIGN KEY (assigned_to) REFERENCES user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT project_comment_thread_assigned_by FOREIGN KEY (assigned_by) REFERENCES user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT project_comment_thread_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createProjectCommentTable = do
  logInfo _CMP_MIGRATION "(Table/ProjectComment) create table"
  let sql =
        "CREATE TABLE project_comment \
        \( \
        \    uuid                uuid        NOT NULL, \
        \    text                text        NOT NULL, \
        \    comment_thread_uuid uuid, \
        \    created_by          uuid, \
        \    created_at          timestamptz NOT NULL, \
        \    updated_at          timestamptz NOT NULL, \
        \    tenant_uuid         uuid        NOT NULL, \
        \    CONSTRAINT project_comment_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT project_comment_comment_thread_uuid FOREIGN KEY (comment_thread_uuid) REFERENCES project_comment_thread (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_comment_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createProjectVersionTable = do
  logInfo _CMP_MIGRATION "(Table/ProjectVersion) create table"
  let sql =
        "CREATE TABLE project_version \
        \( \
        \    uuid               uuid        NOT NULL, \
        \    name               varchar     NOT NULL, \
        \    description        varchar, \
        \    event_uuid         uuid        NOT NULL, \
        \    project_uuid uuid        NOT NULL, \
        \    tenant_uuid        uuid        NOT NULL, \
        \    created_by         uuid, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    CONSTRAINT project_version_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT project_version_event_uuid_fk FOREIGN KEY (event_uuid) REFERENCES project_event (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_version_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_version_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_version_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE SET NULL \
        \);"
  let action conn = execute_ conn sql
  runDB action

createProjectFileTable = do
  logInfo _CMP_MIGRATION "(Table/ProjectFile) create table"
  let sql =
        "CREATE TABLE project_file \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    file_name    varchar     NOT NULL, \
        \    content_type varchar     NOT NULL, \
        \    file_size    bigint      NOT NULL, \
        \    project_uuid uuid        NOT NULL, \
        \    created_by   uuid, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    CONSTRAINT project_file_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT project_file_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT project_file_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT project_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: AppContextM Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Project) create functions"
  createPersistentCommandFromProjectFileDeleteFunction

createPersistentCommandFromProjectFileDeleteFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_project_file_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM create_persistent_command( \
        \            'project_file', \
        \            'deleteFromS3', \
        \            jsonb_build_object('projectUuid', OLD.project_uuid, 'fileUuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: AppContextM Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Project) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_project_file_delete \
        \    AFTER DELETE \
        \    ON project_file \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_project_file_delete();"
  let action conn = execute_ conn sql
  runDB action
