module Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/KnowledgeModel) drop tables"
  let sql = "DROP TABLE IF EXISTS knowledge_model_migration;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/KnowledgeModel) create table"
  let sql =
        "CREATE TABLE knowledge_model_migration \
        \( \
        \    branch_uuid                uuid        NOT NULL, \
        \    metamodel_version          int         NOT NULL, \
        \    migration_state            jsonb       NOT NULL, \
        \    branch_previous_package_id varchar     NOT NULL, \
        \    target_package_id          varchar     NOT NULL, \
        \    branch_events              jsonb       NOT NULL, \
        \    target_package_events      jsonb       NOT NULL, \
        \    result_events              jsonb       NOT NULL, \
        \    current_knowledge_model    jsonb, \
        \    tenant_uuid                uuid        NOT NULL, \
        \    created_at                 timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_migration_pk PRIMARY KEY (branch_uuid, tenant_uuid), \
        \    CONSTRAINT knowledge_model_migration_branch_uuid_fk FOREIGN KEY (branch_uuid, tenant_uuid) REFERENCES branch (uuid, tenant_uuid), \
        \    CONSTRAINT knowledge_model_migration_branch_previous_package_id_fk FOREIGN KEY (branch_previous_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT knowledge_model_migration_target_package_id_fk FOREIGN KEY (target_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT knowledge_model_migration_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
