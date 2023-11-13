module Wizard.Database.Migration.Development.Package.PackageSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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
  let sql = "DROP TABLE IF EXISTS package CASCADE;"
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
        "CREATE TABLE package \
        \( \
        \    id                          varchar     NOT NULL, \
        \    name                        varchar     NOT NULL, \
        \    organization_id             varchar     NOT NULL, \
        \    km_id                       varchar     NOT NULL, \
        \    version                     varchar     NOT NULL, \
        \    metamodel_version           integer     NOT NULL, \
        \    description                 varchar     NOT NULL, \
        \    readme                      varchar     NOT NULL, \
        \    license                     varchar     NOT NULL, \
        \    previous_package_id         varchar, \
        \    fork_of_package_id          varchar, \
        \    merge_checkpoint_package_id varchar, \
        \    events                      json        NOT NULL, \
        \    created_at                  timestamptz NOT NULL, \
        \    tenant_uuid                 uuid        NOT NULL, \
        \    phase                       varchar     NOT NULL, \
        \    non_editable                bool        NOT NULL, \
        \    CONSTRAINT package_pk PRIMARY KEY (id, tenant_uuid), \
        \    CONSTRAINT package_previous_package_id_fk FOREIGN KEY (previous_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT package_fork_of_package_id_fk FOREIGN KEY (fork_of_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT package_merge_checkpoint_package_id_fk FOREIGN KEY (merge_checkpoint_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT package_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \); \
        \ \
        \CREATE INDEX package_organization_id_km_id_index ON package (organization_id, km_id, tenant_uuid); \
        \ \
        \CREATE INDEX package_previous_package_id_index ON package (previous_package_id, tenant_uuid);"
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
        "CREATE or REPLACE FUNCTION get_newest_package(req_organization_id varchar, req_km_id varchar, req_tenant_uuid uuid, req_phase varchar[]) \
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
        \      AND tenant_uuid = req_tenant_uuid \
        \      AND phase = any(req_phase) \
        \    GROUP BY organization_id, km_id; \
        \    RETURN p_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetNewestPackage2Fn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_newest_package_2(req_p_id varchar, req_tenant_uuid uuid, req_phase varchar[]) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \        WHEN req_p_id IS NULL THEN NULL \
        \        ELSE get_newest_package(get_organization_id(req_p_id), get_km_id(req_p_id), req_tenant_uuid, req_phase) \
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
