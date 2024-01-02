module Wizard.Database.Migration.Development.Branch.BranchSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Branch) drop tables"
  let sql =
        "DROP TABLE IF EXISTS branch_data CASCADE; \
        \DROP TABLE IF EXISTS branch CASCADE; "
  let action conn = execute_ conn sql
  runDB action

dropFunctions :: AppContextM Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Branch) drop functions"
  let sql =
        "DROP FUNCTION IF EXISTS get_branch_fork_of_package_id; \
        \DROP FUNCTION IF EXISTS get_branch_state;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createBranchTable
  createBranchDataTable

createBranchTable = do
  logInfo _CMP_MIGRATION "(Table/Branch) create table"
  let sql =
        "CREATE TABLE branch \
        \( \
        \    uuid                uuid        NOT NULL, \
        \    name                varchar     NOT NULL, \
        \    km_id               varchar     NOT NULL, \
        \    previous_package_id varchar, \
        \    created_by          uuid, \
        \    created_at          timestamptz NOT NULL, \
        \    updated_at          timestamptz NOT NULL, \
        \    tenant_uuid         uuid        NOT NULL, \
        \    version             varchar     NOT NULL, \
        \    description         varchar     NOT NULL, \
        \    readme              varchar     NOT NULL, \
        \    license             varchar     NOT NULL, \
        \    CONSTRAINT branch_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT branch_previous_package_id_fk FOREIGN KEY (previous_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT branch_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT branch_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createBranchDataTable = do
  logInfo _CMP_MIGRATION "(Table/BranchData) create table"
  let sql =
        "CREATE TABLE branch_data \
        \( \
        \    branch_uuid       uuid        NOT NULL, \
        \    metamodel_version int         NOT NULL, \
        \    events            json, \
        \    tenant_uuid       uuid        NOT NULL, \
        \    created_at        timestamptz NOT NULL, \
        \    updated_at        timestamptz NOT NULL, \
        \    squashed          bool        NOT NULL, \
        \    CONSTRAINT branch_data_pk PRIMARY KEY (branch_uuid, tenant_uuid), \
        \    CONSTRAINT branch_data_branch_uuid_fk FOREIGN KEY (branch_uuid, tenant_uuid) REFERENCES branch (uuid, tenant_uuid), \
        \    CONSTRAINT branch_data_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: AppContextM Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Branch) create functions"
  createGetBranchForkOfPackageIdFn
  createGetBranchStateFn

createGetBranchForkOfPackageIdFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_branch_fork_of_package_id(tenant_config tenant_config, \
        \                                                         previous_pkg package, \
        \                                                         branch branch) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    fork_of_package_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN branch.previous_package_id IS NULL THEN NULL \
        \               WHEN previous_pkg.organization_id = tenant_config.organization ->> 'organizationId' AND \
        \                    previous_pkg.km_id = branch.km_id THEN previous_pkg.fork_of_package_id \
        \               WHEN True THEN branch.previous_package_id END as fork_of_package_id \
        \    INTO fork_of_package_id; \
        \    RETURN fork_of_package_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetBranchStateFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_branch_state(knowledge_model_migration knowledge_model_migration, \
        \                                           branch_data branch_data, \
        \                                           fork_of_package_id varchar, \
        \                                           tenant_uuid uuid) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN knowledge_model_migration.migration_state ->> 'stateType' IS NOT NULL AND \
        \                    knowledge_model_migration.migration_state ->> 'stateType' != 'CompletedState' THEN 'BSMigrating' \
        \               WHEN json_array_length(branch_data.events) > 0 THEN 'BSEdited' \
        \               WHEN knowledge_model_migration.migration_state ->> 'stateType' IS NOT NULL AND \
        \                    knowledge_model_migration.migration_state ->> 'stateType' = 'CompletedState' THEN 'BSMigrated' \
        \               WHEN fork_of_package_id != get_newest_package_2(fork_of_package_id, tenant_uuid, ARRAY['ReleasedPackagePhase', 'DeprecatedPackagePhase']) THEN 'BSOutdated' \
        \               WHEN True THEN 'BSDefault' END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
