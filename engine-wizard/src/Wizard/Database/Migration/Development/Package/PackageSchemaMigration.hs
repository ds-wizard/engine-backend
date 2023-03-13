module Wizard.Database.Migration.Development.Package.PackageSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Package) started"
  dropFunctions
  dropTables
  createTables
  createFunctions
  logInfo _CMP_MIGRATION "(Table/Package) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Package) drop tables"
  let sql = "drop table if exists package cascade;"
  let action conn = execute_ conn sql
  runDB action

dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Package) drop functions"
  let sql =
        "DROP FUNCTION IF EXISTS get_newest_package; \
        \DROP FUNCTION IF EXISTS get_newest_package_2;\
        \DROP FUNCTION IF EXISTS get_organization_id;\
        \DROP FUNCTION IF EXISTS get_km_id;\
        \DROP FUNCTION IF EXISTS get_package_state;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Package) create table"
  let sql =
        "create table package \
        \ ( \
        \     id                          varchar                  not null, \
        \     name                        varchar                  not null, \
        \     organization_id             varchar                  not null, \
        \     km_id                       varchar                  not null, \
        \     version                     varchar                  not null, \
        \     metamodel_version           integer                  not null, \
        \     description                 varchar                  not null, \
        \     readme                      varchar                  not null, \
        \     license                     varchar                  not null, \
        \     previous_package_id         varchar, \
        \     fork_of_package_id          varchar, \
        \     merge_checkpoint_package_id varchar, \
        \     events                      json                     not null, \
        \     created_at                  timestamp with time zone not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint branch_app_uuid_fk \
        \         references app, \
        \     phase                       varchar                  not null default 'ReleasedPackagePhase' \
        \ ); \
        \alter table package\
        \     add constraint package_pk primary key (id, app_uuid);\
        \create unique index package_id_uindex \
        \     on package (id, app_uuid); \
        \ \
        \create index package_organization_id_km_id_index \
        \     on package (organization_id, km_id, app_uuid); \
        \create index package_previous_package_id_index \
        \     on package (previous_package_id, app_uuid); \
        \ \
        \alter table package \
        \   add constraint package_previous_package_id_fk \
        \      foreign key (previous_package_id, app_uuid) references package (id, app_uuid); \
        \alter table package \
        \   add constraint package_fork_of_package_id_fk \
        \      foreign key (fork_of_package_id, app_uuid) references package (id, app_uuid); \
        \alter table package \
        \   add constraint package_merge_checkpoint_package_id_fk \
        \      foreign key (merge_checkpoint_package_id, app_uuid) references package (id, app_uuid); "
  let action conn = execute_ conn sql
  runDB action

createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Package) create functions"
  createGetNewestPackageFn
  createGetNewestPackage2Fn
  createGetOrganizationIdFn
  createGetKmIdFn
  createGetPackageStateFn

createGetNewestPackageFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_newest_package(req_organization_id varchar, req_km_id varchar, req_app_uuid uuid) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CONCAT(organization_id, ':', km_id, ':', \
        \                  (max(string_to_array(version, '.')::int[]))[1] || \
        \                  '.' || \
        \                  (max(string_to_array(version, '.')::int[]))[2] || \
        \                  '.' || \
        \                  (max(string_to_array(version, '.')::int[]))[3]) \
        \    INTO p_id \
        \    FROM package \
        \    WHERE organization_id = req_organization_id \
        \      AND km_id = req_km_id \
        \      AND app_uuid = req_app_uuid \
        \    GROUP BY organization_id, km_id; \
        \    RETURN p_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetNewestPackage2Fn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_newest_package_2(req_p_id varchar, req_app_uuid uuid) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \        WHEN req_p_id IS NULL THEN NULL \
        \        ELSE get_newest_package(get_organization_id(req_p_id), get_km_id(req_p_id), req_app_uuid) \
        \        END as newest_package_id \
        \    INTO p_id; \
        \    RETURN p_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetOrganizationIdFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_organization_id(req_p_id varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    organization_id varchar; \
        \BEGIN \
        \    SELECT split_part(req_p_id, ':', 1) \
        \    INTO organization_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetKmIdFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_km_id(req_p_id varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    km_id varchar; \
        \BEGIN \
        \    SELECT split_part(req_p_id, ':', 2) \
        \    INTO km_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetPackageStateFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_package_state(remote_version varchar, local_version varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN remote_version IS NULL THEN 'UnknownPackageState' \
        \               WHEN compare_version(remote_version, local_version) = 'LT' THEN 'UnpublishedPackageState' \
        \               WHEN compare_version(remote_version, local_version) = 'EQ' THEN 'UpToDatePackageState' \
        \               WHEN compare_version(remote_version, local_version) = 'GT' THEN 'OutdatedPackageState' \
        \               END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
