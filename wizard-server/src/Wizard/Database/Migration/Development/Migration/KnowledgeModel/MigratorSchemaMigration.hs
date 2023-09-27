module Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Migration/KnowledgeModel) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Migration/KnowledgeModel) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/KnowledgeModel) drop tables"
  let sql = "drop table if exists knowledge_model_migration;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Migration/KnowledgeModel) create table"
  let sql =
        " create table knowledge_model_migration \
        \ ( \
        \     branch_uuid uuid not null, \
        \     metamodel_version int not null, \
        \     migration_state json not null, \
        \     branch_previous_package_id varchar not null, \
        \     target_package_id varchar not null, \
        \     branch_events json not null, \
        \     target_package_events json not null, \
        \     result_events json not null, \
        \     current_knowledge_model json, \
        \     tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null, \
        \     created_at timestamptz not null \
        \ ); \
        \  \
        \ create unique index knowledge_model_migration_branch_uuid_uindex \
        \     on knowledge_model_migration (branch_uuid); \
        \  \
        \ alter table knowledge_model_migration \
        \     add constraint knowledge_model_migration_pk \
        \         primary key (branch_uuid); \
        \  \
        \ alter table knowledge_model_migration \
        \   add constraint knowledge_model_migration_branch_uuid_fk \
        \      foreign key (branch_uuid) references branch; \
        \  \
        \ alter table knowledge_model_migration \
        \   add constraint knowledge_model_migration_branch_previous_package_id_fk \
        \      foreign key (branch_previous_package_id, tenant_uuid) references package (id, tenant_uuid); \
        \  \
        \ alter table knowledge_model_migration \
        \   add constraint knowledge_model_migration_target_package_id_fk \
        \      foreign key (target_package_id, tenant_uuid) references package (id, tenant_uuid); "
  let action conn = execute_ conn sql
  runDB action
