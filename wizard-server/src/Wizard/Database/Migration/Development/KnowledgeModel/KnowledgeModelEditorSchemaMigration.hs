module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelEditor) drop tables"
  let sql =
        "DROP TABLE IF EXISTS knowledge_model_editor_reply CASCADE; \
        \DROP TYPE IF EXISTS knowledge_model_editor_reply_type CASCADE; \
        \DROP TABLE IF EXISTS knowledge_model_editor_event CASCADE; \
        \DROP TABLE IF EXISTS knowledge_model_editor CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropFunctions :: AppContextM Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/KnowledgeModelEditor) drop functions"
  let sql =
        "DROP FUNCTION IF EXISTS get_knowledge_model_editor_fork_of_package_id; \
        \DROP FUNCTION IF EXISTS get_knowledge_model_editor_state;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createKnowledgeModelEditorTable
  createKnowledgeModelEditorEventTable
  createKnowledgeModelEditorReplyTable

createKnowledgeModelEditorTable = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelEditor) create table"
  let sql =
        "CREATE TABLE knowledge_model_editor \
        \( \
        \    uuid                 uuid        NOT NULL, \
        \    name                 varchar     NOT NULL, \
        \    km_id                varchar     NOT NULL, \
        \    previous_package_id  varchar, \
        \    created_by           uuid, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    version              varchar     NOT NULL, \
        \    description          varchar     NOT NULL, \
        \    readme               varchar     NOT NULL, \
        \    license              varchar     NOT NULL, \
        \    metamodel_version    integer     NOT NULL, \
        \    squashed             boolean     NOT NULL, \
        \    CONSTRAINT knowledge_model_editor_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT knowledge_model_editor_previous_package_id_fk FOREIGN KEY (previous_package_id, tenant_uuid) REFERENCES knowledge_model_package (id, tenant_uuid), \
        \    CONSTRAINT knowledge_model_editor_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT knowledge_model_editor_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createKnowledgeModelEditorEventTable = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelEditorEvent) create table"
  let sql =
        "CREATE TABLE IF NOT EXISTS knowledge_model_editor_event \
        \( \
        \    uuid        UUID        NOT NULL, \
        \    parent_uuid UUID        NOT NULL, \
        \    entity_uuid UUID        NOT NULL, \
        \    content     JSONB       NOT NULL, \
        \    editor_uuid UUID        NOT NULL, \
        \    tenant_uuid UUID        NOT NULL, \
        \    created_at  TIMESTAMPTZ NOT NULL, \
        \    CONSTRAINT knowledge_model_editor_event_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT knowledge_model_editor_event_editor_uuid_fk FOREIGN KEY (editor_uuid, tenant_uuid) REFERENCES knowledge_model_editor (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_editor_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createKnowledgeModelEditorReplyTable = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelEditorReply) create table"
  let sql =
        "CREATE TYPE knowledge_model_editor_reply_type AS ENUM ( \
        \    'StringReply',  \
        \    'AnswerReply',  \
        \    'ItemSelectReply',  \
        \    'FileReply', \
        \    'IntegrationReply',  \
        \    'MultiChoiceReply',  \
        \    'ItemListReply' \
        \); \
        \ \
        \CREATE TABLE IF NOT EXISTS knowledge_model_editor_reply \
        \( \
        \    path        text                              NOT NULL, \
        \    value_type  knowledge_model_editor_reply_type NOT NULL, \
        \    value       text[], \
        \    value_id    text, \
        \    value_raw   jsonb, \
        \    editor_uuid uuid                              NOT NULL, \
        \    created_by  jsonb, \
        \    tenant_uuid uuid                              NOT NULL, \
        \    created_at  timestamptz                       NOT NULL, \
        \    CONSTRAINT knowledge_model_editor_reply_pk PRIMARY KEY (editor_uuid, path, tenant_uuid), \
        \    CONSTRAINT knowledge_model_editor_reply_editor_uuid FOREIGN KEY (editor_uuid, tenant_uuid) REFERENCES knowledge_model_editor (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_editor_reply_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: AppContextM Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/KnowledgeModelEditor) create functions"
  createGetKnowledgeModelEditorForkOfPackageIdFn
  createGetKnowledgeModelEditorStateFn

createGetKnowledgeModelEditorForkOfPackageIdFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_knowledge_model_editor_fork_of_package_id(config_organization config_organization, \
        \                                                                                         previous_pkg knowledge_model_package, \
        \                                                                                         knowledge_model_editor knowledge_model_editor) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    fork_of_package_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN knowledge_model_editor.previous_package_id IS NULL THEN NULL \
        \               WHEN previous_pkg.organization_id = config_organization.organization_id AND \
        \                    previous_pkg.km_id = knowledge_model_editor.km_id THEN previous_pkg.fork_of_package_id \
        \               WHEN True THEN knowledge_model_editor.previous_package_id END as fork_of_package_id \
        \    INTO fork_of_package_id; \
        \    RETURN fork_of_package_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetKnowledgeModelEditorStateFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_knowledge_model_editor_state(knowledge_model_editor_uuid uuid, \
        \                                                            knowledge_model_migration knowledge_model_migration, \
        \                                                            fork_of_package_id varchar, \
        \                                                            editor_tenant_uuid uuid) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND \
        \                    knowledge_model_migration.state ->> 'type' != 'CompletedKnowledgeModelMigrationState' THEN 'MigratingKnowledgeModelEditorState' \
        \               WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND \
        \                    knowledge_model_migration.state ->> 'type' = 'CompletedKnowledgeModelMigrationState' THEN 'MigratedKnowledgeModelEditorState' \
        \               WHEN (SELECT COUNT(*) FROM knowledge_model_editor_event e WHERE e.tenant_uuid = editor_tenant_uuid AND e.editor_uuid = knowledge_model_editor_uuid) > 0 THEN 'EditedKnowledgeModelEditorState' \
        \               WHEN fork_of_package_id != get_newest_knowledge_model_package_2(fork_of_package_id, editor_tenant_uuid, ARRAY['ReleasedKnowledgeModelPackagePhase', 'DeprecatedKnowledgeModelPackagePhase']) THEN 'OutdatedKnowledgeModelEditorState' \
        \               WHEN True THEN 'DefaultKnowledgeModelEditorState' END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
