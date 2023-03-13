module Wizard.Database.Migration.Development.Branch.BranchSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Branch) started"
  dropFunctions
  dropTables
  createTables
  createFunctions
  logInfo _CMP_MIGRATION "(Table/Branch) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Branch) drop tables"
  let sql =
        "DROP TABLE IF EXISTS branch_data CASCADE; \
        \DROP TABLE IF EXISTS branch CASCADE; "
  let action conn = execute_ conn sql
  runDB action

dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Branch) drop functions"
  let sql =
        "DROP FUNCTION IF EXISTS get_branch_fork_of_package_id; \
        \DROP FUNCTION IF EXISTS get_branch_state;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Branch) create table"
  let sql =
        "create table branch \
        \ ( \
        \     uuid uuid not null \
        \         constraint branch_pk \
        \             primary key, \
        \     name varchar not null, \
        \     km_id varchar not null, \
        \     previous_package_id varchar, \
        \     created_by uuid not null \
        \         constraint branch_user_entity_uuid_fk \
        \             references user_entity, \
        \     created_at timestamptz not null, \
        \     updated_at timestamptz not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint branch_app_uuid_fk \
        \         references app, \
        \     version varchar not null, \
        \     description varchar not null, \
        \     readme varchar not null, \
        \     license varchar not null \
        \); \
        \  \
        \ create unique index branch_uuid_uindex \
        \     on branch (uuid); \
        \ alter table branch \
        \    add constraint branch_package_id_fk \
        \        foreign key (previous_package_id, app_uuid) references package (id, app_uuid); \
        \   \
        \create table branch_data \
        \ ( \
        \     branch_uuid uuid not null \
        \         constraint branch_data_pk \
        \             primary key \
        \         constraint branch_data_branch_uuid_fk \
        \             references branch, \
        \     metamodel_version int not null, \
        \     events json, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint branch_app_uuid_fk \
        \         references app, \
        \     created_at timestamptz not null, \
        \     updated_at timestamptz not null \
        \); \
        \  \
        \create unique index branch_data_branch_uuid_uindex \
        \     on branch_data (branch_uuid); "
  let action conn = execute_ conn sql
  runDB action

createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Branch) create functions"
  createGetBranchForkOfPackageIdFn
  createGetBranchStateFn

createGetBranchForkOfPackageIdFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_branch_fork_of_package_id(app_config app_config, \
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
        \               WHEN previous_pkg.organization_id = app_config.organization ->> 'organizationId' AND \
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
        \                                           app_uuid uuid) \
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
        \               WHEN fork_of_package_id != get_newest_package_2(fork_of_package_id, app_uuid) THEN 'BSOutdated' \
        \               WHEN True THEN 'BSDefault' END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
