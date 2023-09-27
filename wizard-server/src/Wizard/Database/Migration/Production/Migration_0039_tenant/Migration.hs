module Wizard.Database.Migration.Production.Migration_0039_tenant.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 39, mmName = "Tenant", mmDescription = "Rename App to Tenant"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE app RENAME TO tenant; \
        \ALTER TABLE tenant RENAME app_id TO tenant_id; \
        \ALTER TABLE tenant RENAME CONSTRAINT app_pk TO tenant_pk; \
        \ALTER INDEX app_uuid_uindex RENAME TO tenant_uuid_uindex; \
        \ \
        \ALTER TABLE app_config RENAME TO tenant_config; \
        \ALTER TABLE tenant_config RENAME CONSTRAINT app_config_pk TO tenant_config_pk; \
        \ALTER TABLE tenant_config RENAME CONSTRAINT app_config_instance_config_mail_uuid_fk TO tenant_config_instance_config_mail_uuid_fk; \
        \ \
        \ALTER TABLE app_limit RENAME TO tenant_limit_bundle; \
        \ALTER TABLE tenant_limit_bundle RENAME CONSTRAINT app_limit_pk TO tenant_limit_bundle_pk; \
        \ALTER INDEX app_limit_uuid_uindex RENAME TO tenant_limit_bundle_uuid_uindex; \
        \ \
        \ALTER TABLE app_plan RENAME TO tenant_plan; \
        \ALTER TABLE tenant_plan RENAME CONSTRAINT app_plan_pk TO tenant_plan_pk; \
        \ALTER INDEX app_plan_uuid_uindex RENAME TO tenant_plan_uuid_uindex; \
        \ALTER TABLE tenant_plan ADD CONSTRAINT tenant_plan_tenant_uuid_fk FOREIGN KEY (app_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE tenant_plan RENAME app_uuid TO tenant_uuid; \
        \ \
        \ALTER TABLE action_key RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE action_key RENAME CONSTRAINT action_key_app_uuid_fk TO action_key_tenant_uuid_fk; \
        \ \
        \ALTER TABLE audit RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE audit RENAME CONSTRAINT audit_app_uuid_fk TO audit_tenant_uuid_fk; \
        \ \
        \ALTER TABLE branch RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE branch RENAME CONSTRAINT branch_app_uuid_fk TO branch_tenant_uuid_fk; \
        \ \
        \ALTER TABLE branch_data RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE branch_data RENAME CONSTRAINT branch_app_uuid_fk TO branch_data_tenant_uuid_fk; \
        \ \
        \ALTER TABLE document RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE document RENAME CONSTRAINT document_app_uuid_fk TO document_tenant_uuid_fk; \
        \ \
        \ALTER TABLE document_template RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE document_template RENAME CONSTRAINT document_template_app_uuid_fk TO document_template_tenant_uuid_fk; \
        \ \
        \ALTER TABLE document_template_asset RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE document_template_asset RENAME CONSTRAINT document_template_asset_app_uuid_fk TO document_template_asset_tenant_uuid_fk; \
        \ \
        \ALTER TABLE document_template_draft_data RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE document_template_draft_data RENAME CONSTRAINT document_template_draft_data_app_uuid_fk TO document_template_draft_data_tenant_uuid_fk; \
        \ \
        \ALTER TABLE document_template_file RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE document_template_file RENAME CONSTRAINT document_template_file_app_uuid_fk TO document_template_file_tenant_uuid_fk; \
        \ \
        \ALTER TABLE feedback RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE feedback RENAME CONSTRAINT feedback_app_uuid_fk TO feedback_tenant_uuid_fk; \
        \ \
        \ALTER TABLE knowledge_model_migration RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE knowledge_model_migration RENAME CONSTRAINT knowledge_model_migration_app_uuid_fk TO knowledge_model_migration_tenant_uuid_fk; \
        \ \
        \ALTER TABLE locale RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE locale RENAME CONSTRAINT locale_app_uuid_fk TO locale_tenant_uuid_fk; \
        \ \
        \ALTER TABLE package RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE package RENAME CONSTRAINT package_app_uuid_fk TO package_tenant_uuid_fk; \
        \ \
        \ALTER TABLE persistent_command RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE persistent_command RENAME CONSTRAINT persistent_command_app_uuid_fk TO persistent_command_tenant_uuid_fk; \
        \ \
        \ALTER TABLE prefab RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE prefab RENAME CONSTRAINT prefab_app_uuid_fk TO prefab_tenant_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE questionnaire RENAME CONSTRAINT questionnaire_app_uuid_fk TO questionnaire_tenant_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_importer RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE questionnaire_importer RENAME CONSTRAINT questionnaire_importer_app_uuid_fk TO questionnaire_importer_tenant_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_migration RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE questionnaire_migration RENAME CONSTRAINT questionnaire_migration_app_uuid_fk TO questionnaire_migration_tenant_uuid_fk; \
        \ \
        \ALTER TABLE submission RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE submission RENAME CONSTRAINT submission_app_uuid_fk TO submission_tenant_uuid_fk; \
        \ \
        \ALTER TABLE temporary_file RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE temporary_file RENAME CONSTRAINT temporary_file_app_uuid_fk TO temporary_file_tenant_uuid_fk; \
        \ \
        \ALTER TABLE user_entity RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE user_entity RENAME CONSTRAINT user_entity_app_uuid_fk TO user_entity_tenant_uuid_fk; \
        \ \
        \ALTER TABLE user_token RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE user_token RENAME CONSTRAINT user_entity_app_uuid_fk TO user_token_tenant_uuid_fk; \
        \ \
        \UPDATE user_entity \
        \SET permissions = '{ TENANT_PERM, DEV_PERM , UM_PERM, KM_PERM, KM_UPGRADE_PERM, KM_PUBLISH_PERM, PM_READ_PERM, PM_WRITE_PERM, QTN_PERM, QTN_IMPORTER_PERM, QTN_TML_PERM, DOC_TML_READ_PERM, CFG_PERM, SUBM_PERM, DOC_TML_WRITE_PERM, DOC_PERM, LOC_PERM}' \
        \WHERE uuid = '00000000-0000-0000-0000-000000000000' AND tenant_uuid = '00000000-0000-0000-0000-000000000000'; \
        \ \
        \UPDATE tenant \
        \SET name = 'Default Tenant' \
        \WHERE uuid = '00000000-0000-0000-0000-000000000000'; \
        \ \
        \DROP FUNCTION get_newest_package_2(req_p_id varchar, req_app_uuid uuid, req_phase character varying[]); \
        \DROP FUNCTION get_newest_package_2(req_p_id varchar, req_app_uuid uuid); \
        \DROP FUNCTION get_newest_package(req_organization_id varchar, req_km_id varchar, req_app_uuid uuid, req_phase character varying[]); \
        \DROP FUNCTION get_newest_package(req_organization_id varchar, req_km_id varchar, req_app_uuid uuid); \
        \DROP FUNCTION get_branch_state(knowledge_model_migration knowledge_model_migration, branch_data branch_data, fork_of_package_id varchar, app_uuid uuid); \
        \ \
        \CREATE or REPLACE FUNCTION get_newest_package(req_organization_id varchar, req_km_id varchar, req_tenant_uuid uuid, req_phase varchar[]) \
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
        \$$; \
        \ \
        \CREATE or REPLACE FUNCTION get_newest_package(req_organization_id varchar, req_km_id varchar, req_tenant_uuid uuid) \
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
        \    GROUP BY organization_id, km_id; \
        \    RETURN p_id; \
        \END; \
        \$$; \
        \ \
        \CREATE or REPLACE FUNCTION get_newest_package_2(req_p_id varchar, req_tenant_uuid uuid, req_phase varchar[]) \
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
        \$$; \
        \ \
        \CREATE or REPLACE FUNCTION get_newest_package_2(req_p_id varchar, req_tenant_uuid uuid) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \        WHEN req_p_id IS NULL THEN NULL \
        \        ELSE get_newest_package(get_organization_id(req_p_id), get_km_id(req_p_id), req_tenant_uuid) \
        \        END as newest_package_id \
        \    INTO p_id; \
        \    RETURN p_id; \
        \END; \
        \$$; \
        \ \
        \CREATE or REPLACE FUNCTION get_branch_state(knowledge_model_migration knowledge_model_migration, \
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
        \               WHEN fork_of_package_id != get_newest_package_2(fork_of_package_id, tenant_uuid) THEN 'BSOutdated' \
        \               WHEN True THEN 'BSDefault' END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
