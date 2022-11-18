module Wizard.Database.Migration.Production.Migration_0021_qtnImporter.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 21
    , mmName = "Add Questionnaire Importer"
    , mmDescription = "Add qtn importer table and QTN_IMPORTER_PERM to user"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createQtnImporterTable dbPool
  addQtnImporterPermission dbPool
  createGetNewestPackageFn dbPool
  createGetNewestPackage2Fn dbPool
  createGetOrganizationIdFn dbPool
  createGetKmIdFn dbPool
  createGetBranchForkOfPackageIdFn dbPool
  createGetBranchStateFn dbPool
  createRegistryOrganizationTable dbPool
  createRegistryPackageTable dbPool
  createRegistryTemplateTable dbPool
  return Nothing

createQtnImporterTable dbPool = do
  let sql =
        "CREATE TABLE questionnaire_importer \
        \ ( \
        \     id                     varchar                  not null, \
        \     name                   varchar                  not null, \
        \     organization_id        varchar                  not null, \
        \     importer_id            varchar                  not null, \
        \     version                varchar                  not null, \
        \     metamodel_version      integer                  not null, \
        \     description            varchar                  not null, \
        \     readme                 varchar                  not null, \
        \     license                varchar                  not null, \
        \     allowed_packages       json                     not null, \
        \     url                    varchar, \
        \     enabled                bool                     not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint questionnaire_importer_app_uuid_fk \
        \         references app, \
        \     created_at             timestamp with time zone not null, \
        \     updated_at             timestamp with time zone not null \
        \ ); \
        \ \
        \ALTER TABLE questionnaire_importer \
        \    ADD CONSTRAINT questionnaire_importer_pk PRIMARY KEY (id, app_uuid); \
        \create unique index questionnaire_importer_id_uindex \
        \     on questionnaire_importer (id, app_uuid); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addQtnImporterPermission dbPool = do
  let sql =
        "UPDATE user_entity set permissions = permissions || '{QTN_IMPORTER_PERM}' WHERE role = 'admin' OR role = 'dataSteward'"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createGetNewestPackageFn dbPool = do
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
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createGetNewestPackage2Fn dbPool = do
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
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createGetOrganizationIdFn dbPool = do
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
        \    RETURN organization_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createGetKmIdFn dbPool = do
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
        \    RETURN km_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createGetBranchForkOfPackageIdFn dbPool = do
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
  liftIO $ withResource dbPool action
  return Nothing

createGetBranchStateFn dbPool = do
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
  liftIO $ withResource dbPool action
  return Nothing

createRegistryOrganizationTable dbPool = do
  let sql =
        "CREATE TABLE registry_organization \
        \ ( \
        \     organization_id varchar                  not null, \
        \     name            varchar                  not null, \
        \     logo            varchar, \
        \     created_at      timestamp with time zone not null \
        \ ); \
        \ALTER TABLE registry_organization\
        \     ADD CONSTRAINT registry_organization_pk PRIMARY KEY (organization_id);\
        \CREATE UNIQUE INDEX registry_organization_id_uindex \
        \     ON registry_organization (organization_id); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createRegistryPackageTable dbPool = do
  let sql =
        "CREATE TABLE registry_package \
        \ ( \
        \     organization_id varchar                  not null, \
        \     km_id           varchar                  not null, \
        \     remote_version  varchar                  not null, \
        \     created_at      timestamp with time zone not null \
        \ ); \
        \ALTER TABLE registry_package\
        \     ADD CONSTRAINT registry_package_pk PRIMARY KEY (organization_id, km_id);\
        \CREATE UNIQUE INDEX registry_package_id_uindex \
        \     ON registry_package (organization_id, km_id); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createRegistryTemplateTable dbPool = do
  let sql =
        "CREATE TABLE registry_template \
        \ ( \
        \     organization_id varchar                  not null, \
        \     template_id     varchar                  not null, \
        \     remote_version  varchar                  not null, \
        \     created_at      timestamp with time zone not null \
        \ ); \
        \ALTER TABLE registry_template\
        \     ADD CONSTRAINT registry_template_pk PRIMARY KEY (organization_id, template_id);\
        \CREATE UNIQUE INDEX registry_template_id_uindex \
        \     ON registry_template (organization_id, template_id); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
