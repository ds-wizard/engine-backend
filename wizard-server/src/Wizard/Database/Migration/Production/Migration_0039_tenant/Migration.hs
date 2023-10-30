module Wizard.Database.Migration.Production.Migration_0039_tenant.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 39, mmName = "Tenant", mmDescription = "Rename App to Tenant"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  renameToTenant dbPool
  renameDocumentCreatorUuid dbPool
  removeConstrains dbPool
  refactorQuestionnaireAcl dbPool
  addConstrains dbPool

renameToTenant :: Pool Connection -> LoggingT IO (Maybe Error)
renameToTenant dbPool = do
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

renameDocumentCreatorUuid :: Pool Connection -> LoggingT IO (Maybe Error)
renameDocumentCreatorUuid dbPool = do
  let sql = "ALTER TABLE document rename column creator_uuid to created_by;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

removeConstrains :: Pool Connection -> LoggingT IO (Maybe Error)
removeConstrains dbPool = do
  let sql =
        "ALTER TABLE action_key DROP CONSTRAINT action_key_pk; \
        \ALTER TABLE action_key DROP CONSTRAINT action_key_user_entity_uuid_fk; \
        \ALTER TABLE action_key DROP CONSTRAINT action_key_tenant_uuid_fk; \
        \ALTER TABLE audit DROP CONSTRAINT audit_pk; \
        \ALTER TABLE audit DROP CONSTRAINT audit_user_entity_uuid_fk; \
        \ALTER TABLE audit DROP CONSTRAINT audit_tenant_uuid_fk; \
        \ALTER TABLE knowledge_model_migration DROP CONSTRAINT knowledge_model_migration_pk; \
        \ALTER TABLE knowledge_model_migration DROP CONSTRAINT knowledge_model_migration_branch_previous_package_id_fk; \
        \ALTER TABLE knowledge_model_migration DROP CONSTRAINT knowledge_model_migration_branch_uuid_fk; \
        \ALTER TABLE knowledge_model_migration DROP CONSTRAINT knowledge_model_migration_target_package_id_fk; \
        \ALTER TABLE knowledge_model_migration DROP CONSTRAINT knowledge_model_migration_tenant_uuid_fk; \
        \ALTER TABLE branch_data DROP CONSTRAINT branch_data_pk; \
        \ALTER TABLE branch_data DROP CONSTRAINT branch_data_branch_uuid_fk; \
        \ALTER TABLE branch_data DROP CONSTRAINT branch_data_tenant_uuid_fk; \
        \ALTER TABLE branch DROP CONSTRAINT branch_pk; \
        \ALTER TABLE branch DROP CONSTRAINT branch_package_id_fk; \
        \ALTER TABLE branch DROP CONSTRAINT branch_tenant_uuid_fk; \
        \ALTER TABLE branch DROP CONSTRAINT branch_user_entity_uuid_fk; \
        \ALTER TABLE submission DROP CONSTRAINT submission_pk; \
        \ALTER TABLE submission DROP CONSTRAINT submission_created_by_fk; \
        \ALTER TABLE submission DROP CONSTRAINT submission_document_uuid_fk; \
        \ALTER TABLE submission DROP CONSTRAINT submission_tenant_uuid_fk; \
        \ALTER TABLE document DROP CONSTRAINT document_pk; \
        \ALTER TABLE document DROP CONSTRAINT document_questionnaire_uuid_fk; \
        \ALTER TABLE document DROP CONSTRAINT document_user_entity_uuid_fk; \
        \ALTER TABLE document DROP CONSTRAINT document_tenant_uuid_fk; \
        \ALTER TABLE document DROP CONSTRAINT document_template_id_fk; \
        \ALTER TABLE document_template_asset DROP CONSTRAINT document_template_asset_pk; \
        \ALTER TABLE document_template_asset DROP CONSTRAINT document_template_asset_template_id_app_uuid_fk; \
        \ALTER TABLE document_template_asset DROP CONSTRAINT document_template_asset_tenant_uuid_fk; \
        \ALTER TABLE document_template_draft_data DROP CONSTRAINT document_template_draft_data_pk; \
        \ALTER TABLE document_template_draft_data DROP CONSTRAINT document_template_draft_data_document_template_id_fk; \
        \ALTER TABLE document_template_draft_data DROP CONSTRAINT document_template_draft_data_tenant_uuid_fk; \
        \ALTER TABLE document_template_file DROP CONSTRAINT document_template_file_pk; \
        \ALTER TABLE document_template_file DROP CONSTRAINT document_template_file_template_id_fk; \
        \ALTER TABLE document_template_file DROP CONSTRAINT document_template_file_tenant_uuid_fk; \
        \ALTER TABLE feedback DROP CONSTRAINT feedback_pk; \
        \ALTER TABLE feedback DROP CONSTRAINT feedback_package_id_fk; \
        \ALTER TABLE feedback DROP CONSTRAINT feedback_tenant_uuid_fk; \
        \ALTER TABLE locale DROP CONSTRAINT locale_pk; \
        \ALTER TABLE locale DROP CONSTRAINT locale_tenant_uuid_fk; \
        \ALTER TABLE persistent_command DROP CONSTRAINT persistent_command_pk; \
        \ALTER TABLE persistent_command DROP CONSTRAINT persistent_command_created_by_fk; \
        \ALTER TABLE persistent_command DROP CONSTRAINT persistent_command_tenant_uuid_fk; \
        \ALTER TABLE prefab DROP CONSTRAINT prefab_pk; \
        \ALTER TABLE prefab DROP CONSTRAINT prefab_tenant_uuid_fk; \
        \ALTER TABLE questionnaire_acl_group DROP CONSTRAINT questionnaire_acl_group_pk; \
        \ALTER TABLE questionnaire_acl_group DROP CONSTRAINT questionnaire_acl_group_group_id_fk; \
        \ALTER TABLE questionnaire_acl_group DROP CONSTRAINT questionnaire_acl_group_questionnaire_uuid_fk; \
        \ALTER TABLE questionnaire_acl_user DROP CONSTRAINT questionnaire_acl_user_user_uuid_fk; \
        \ALTER TABLE questionnaire_acl_user DROP CONSTRAINT questionnaire_acl_user_questionnaire_uuid_fk; \
        \ALTER TABLE questionnaire_migration DROP CONSTRAINT questionnaire_migration_pk; \
        \ALTER TABLE questionnaire_migration DROP CONSTRAINT questionnaire_migration_new_questionnaire_uuid_fk; \
        \ALTER TABLE questionnaire_migration DROP CONSTRAINT questionnaire_migration_old_questionnaire_uuid_fk; \
        \ALTER TABLE questionnaire_migration DROP CONSTRAINT questionnaire_migration_tenant_uuid_fk; \
        \ALTER TABLE questionnaire_comment DROP CONSTRAINT questionnaire_comment_pk; \
        \ALTER TABLE questionnaire_comment DROP CONSTRAINT questionnaire_comment_questionnaire_comment_thread_uuid_fk; \
        \ALTER TABLE questionnaire_comment DROP CONSTRAINT questionnaire_comment_user_entity_uuid_fk; \
        \ALTER TABLE questionnaire_comment_thread DROP CONSTRAINT questionnaire_comment_thread_pk; \
        \ALTER TABLE questionnaire_comment_thread DROP CONSTRAINT questionnaire_comment_thread_questionnaire_uuid_fk; \
        \ALTER TABLE questionnaire_comment_thread DROP CONSTRAINT questionnaire_comment_thread_user_entity_uuid_fk; \
        \ALTER TABLE questionnaire_importer DROP CONSTRAINT questionnaire_importer_pk; \
        \ALTER TABLE questionnaire_importer DROP CONSTRAINT questionnaire_importer_tenant_uuid_fk; \
        \ALTER TABLE questionnaire DROP CONSTRAINT questionnaire_pk; \
        \ALTER TABLE questionnaire DROP CONSTRAINT questionnaire_package_id_fk; \
        \ALTER TABLE questionnaire DROP CONSTRAINT questionnaire_template_id_fk; \
        \ALTER TABLE questionnaire DROP CONSTRAINT questionnaire_tenant_uuid_fk; \
        \ALTER TABLE questionnaire DROP CONSTRAINT questionnaire_user_entity_uuid_fk; \
        \ALTER TABLE document_template DROP CONSTRAINT document_template_pk; \
        \ALTER TABLE document_template DROP CONSTRAINT document_template_tenant_uuid_fk; \
        \ALTER TABLE temporary_file DROP CONSTRAINT temporary_file_pk; \
        \ALTER TABLE temporary_file DROP CONSTRAINT temporary_file_tenant_uuid_fk; \
        \ALTER TABLE temporary_file DROP CONSTRAINT temporary_file_user_entity_uuid_fk; \
        \ALTER TABLE package DROP CONSTRAINT package_previous_package_id_fk; \
        \ALTER TABLE package DROP CONSTRAINT package_pk; \
        \ALTER TABLE package DROP CONSTRAINT package_tenant_uuid_fk; \
        \ALTER TABLE tenant_plan DROP CONSTRAINT tenant_plan_pk; \
        \ALTER TABLE tenant_plan DROP CONSTRAINT tenant_plan_tenant_uuid_fk; \
        \ALTER TABLE user_token DROP CONSTRAINT user_token_pk; \
        \ALTER TABLE user_token DROP CONSTRAINT user_token_tenant_uuid_fk; \
        \ALTER TABLE user_token DROP CONSTRAINT user_token_user_uuid_fk; \
        \ALTER TABLE user_entity DROP CONSTRAINT user_pk; \
        \ALTER TABLE user_entity DROP CONSTRAINT user_entity_tenant_uuid_fk;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

refactorQuestionnaireAcl :: Pool Connection -> LoggingT IO (Maybe Error)
refactorQuestionnaireAcl dbPool = do
  let sql =
        "DROP TABLE questionnaire_acl_group; \
        \DROP TABLE acl_group; \
        \ \
        \CREATE TABLE user_group \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    name        varchar     NOT NULL, \
        \    description varchar, \
        \    private     boolean     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  TIMESTAMPTZ NOT NULL, \
        \    updated_at  TIMESTAMPTZ NOT NULL \
        \); \
        \ \
        \CREATE TABLE user_group_membership \
        \( \
        \    user_group_uuid uuid        NOT NULL, \
        \    user_uuid       uuid        NOT NULL, \
        \    type            varchar     NOT NULL, \
        \    tenant_uuid     uuid        NOT NULL, \
        \    created_at      TIMESTAMPTZ NOT NULL, \
        \    updated_at      TIMESTAMPTZ NOT NULL \
        \); \
        \ \
        \CREATE TABLE questionnaire_perm_group \
        \( \
        \    questionnaire_uuid uuid   NOT NULL, \
        \    user_group_uuid    uuid   NOT NULL, \
        \    perms              text[] NOT NULL, \
        \    tenant_uuid        uuid   NOT NULL \
        \); \
        \ \
        \CREATE TABLE questionnaire_perm_user \
        \( \
        \    questionnaire_uuid uuid   NOT NULL, \
        \    user_uuid          uuid   NOT NULL, \
        \    perms              text[] NOT NULL, \
        \    tenant_uuid        uuid   NOT NULL \
        \); \
        \ \
        \INSERT INTO questionnaire_perm_user (questionnaire_uuid, user_uuid, perms, tenant_uuid) \
        \SELECT qtn_acl_user.questionnaire_uuid, qtn_acl_user.user_uuid, qtn_acl_user.perms, qtn.tenant_uuid \
        \FROM questionnaire_acl_user qtn_acl_user \
        \JOIN questionnaire qtn ON qtn.uuid = qtn_acl_user.questionnaire_uuid; \
        \ \
        \DROP TABLE questionnaire_acl_user; \
        \ \
        \ALTER TABLE user_entity \
        \    DROP COLUMN groups;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addConstrains :: Pool Connection -> LoggingT IO (Maybe Error)
addConstrains dbPool = do
  let sql =
        "ALTER TABLE tenant_plan ADD CONSTRAINT tenant_plan_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE tenant_plan ADD CONSTRAINT tenant_plan_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE user_entity ADD CONSTRAINT user_entity_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE user_entity ADD CONSTRAINT user_entity_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE user_token ADD CONSTRAINT user_token_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE user_token ADD CONSTRAINT user_token_user_uuid_fk FOREIGN KEY (user_uuid, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE user_token ADD CONSTRAINT user_token_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE user_group ADD CONSTRAINT user_group_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE user_group ADD CONSTRAINT user_group_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE user_group_membership ADD CONSTRAINT user_group_membership_pk PRIMARY KEY (user_group_uuid, user_uuid, tenant_uuid); \
        \ALTER TABLE user_group_membership ADD CONSTRAINT user_group_membership_user_group_uuid_fk FOREIGN KEY (user_group_uuid, tenant_uuid) REFERENCES user_group (uuid, tenant_uuid); \
        \ALTER TABLE user_group_membership ADD CONSTRAINT user_group_membership_user_uuid_fk FOREIGN KEY (user_uuid, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE user_group_membership ADD CONSTRAINT user_group_membership_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE package ADD CONSTRAINT package_pk PRIMARY KEY (id, tenant_uuid); \
        \ALTER TABLE package ADD CONSTRAINT package_previous_package_id_fk FOREIGN KEY (previous_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid); \
        \ALTER TABLE package ADD CONSTRAINT package_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE temporary_file ADD CONSTRAINT temporary_file_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE temporary_file ADD CONSTRAINT temporary_file_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE temporary_file ADD CONSTRAINT temporary_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE document_template ADD CONSTRAINT document_template_pk PRIMARY KEY (id, tenant_uuid); \
        \ALTER TABLE document_template ADD CONSTRAINT document_template_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE questionnaire ADD CONSTRAINT questionnaire_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire ADD CONSTRAINT questionnaire_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES package (id, tenant_uuid); \
        \ALTER TABLE questionnaire ADD CONSTRAINT questionnaire_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid); \
        \ALTER TABLE questionnaire ADD CONSTRAINT questionnaire_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire ADD CONSTRAINT questionnaire_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE questionnaire_importer ADD CONSTRAINT questionnaire_importer_pk PRIMARY KEY (id, tenant_uuid); \
        \ALTER TABLE questionnaire_importer ADD CONSTRAINT questionnaire_importer_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE questionnaire_comment ADD CONSTRAINT questionnaire_comment_pk PRIMARY KEY (uuid, comment_thread_uuid); \
        \ALTER TABLE questionnaire_comment_thread ADD CONSTRAINT questionnaire_comment_thread_pk PRIMARY KEY (uuid, questionnaire_uuid); \
        \ALTER TABLE questionnaire_perm_user ADD CONSTRAINT questionnaire_perm_user_pk PRIMARY KEY (user_uuid, questionnaire_uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_perm_user ADD CONSTRAINT questionnaire_perm_user_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_perm_user ADD CONSTRAINT questionnaire_perm_user_user_uuid_fk FOREIGN KEY (user_uuid, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_perm_user ADD CONSTRAINT questionnaire_perm_user_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE questionnaire_perm_group ADD CONSTRAINT questionnaire_perm_group_pk PRIMARY KEY (user_group_uuid, questionnaire_uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_perm_group ADD CONSTRAINT questionnaire_perm_group_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_perm_group ADD CONSTRAINT questionnaire_perm_group_user_group_uuid_fk FOREIGN KEY (user_group_uuid, tenant_uuid) REFERENCES user_group (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_perm_group ADD CONSTRAINT questionnaire_perm_group_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE questionnaire_migration ADD CONSTRAINT questionnaire_migration_pk PRIMARY KEY (old_questionnaire_uuid, new_questionnaire_uuid); \
        \ALTER TABLE questionnaire_migration ADD CONSTRAINT questionnaire_migration_old_questionnaire_uuid_fk FOREIGN KEY (old_questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_migration ADD CONSTRAINT questionnaire_migration_new_questionnaire_uuid_fk FOREIGN KEY (new_questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid); \
        \ALTER TABLE questionnaire_migration ADD CONSTRAINT questionnaire_migration_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE prefab ADD CONSTRAINT prefab_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE prefab ADD CONSTRAINT prefab_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE persistent_command ADD CONSTRAINT persistent_command_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE persistent_command ADD CONSTRAINT persistent_command_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE persistent_command ADD CONSTRAINT persistent_command_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE locale ADD CONSTRAINT locale_pk PRIMARY KEY (id, tenant_uuid); \
        \ALTER TABLE locale ADD CONSTRAINT locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE feedback ADD CONSTRAINT feedback_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE feedback ADD CONSTRAINT feedback_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES package (id, tenant_uuid); \
        \ALTER TABLE feedback ADD CONSTRAINT feedback_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE document_template_file ADD CONSTRAINT document_template_file_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE document_template_file ADD CONSTRAINT document_template_file_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid); \
        \ALTER TABLE document_template_file ADD CONSTRAINT document_template_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE document_template_asset ADD CONSTRAINT document_template_asset_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE document_template_asset ADD CONSTRAINT document_template_asset_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid); \
        \ALTER TABLE document_template_asset ADD CONSTRAINT document_template_asset_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE document_template_draft_data ADD CONSTRAINT document_template_draft_data_pk PRIMARY KEY (document_template_id, tenant_uuid); \
        \ALTER TABLE document_template_draft_data ADD CONSTRAINT document_template_draft_data_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid); \
        \ALTER TABLE document_template_draft_data ADD CONSTRAINT document_template_draft_data_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE document ADD CONSTRAINT document_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE document ADD CONSTRAINT document_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid); \
        \ALTER TABLE document ADD CONSTRAINT document_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid); \
        \ALTER TABLE document ADD CONSTRAINT document_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE document ADD CONSTRAINT document_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE submission ADD CONSTRAINT submission_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE submission ADD CONSTRAINT submission_document_uuid_fk FOREIGN KEY (document_uuid, tenant_uuid) REFERENCES document (uuid, tenant_uuid); \
        \ALTER TABLE submission ADD CONSTRAINT submission_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE submission ADD CONSTRAINT submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE branch ADD CONSTRAINT branch_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE branch ADD CONSTRAINT branch_previous_package_id_fk FOREIGN KEY (previous_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid); \
        \ALTER TABLE branch ADD CONSTRAINT branch_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE branch ADD CONSTRAINT branch_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE branch_data ADD CONSTRAINT branch_data_pk PRIMARY KEY (branch_uuid, tenant_uuid); \
        \ALTER TABLE branch_data ADD CONSTRAINT branch_data_branch_uuid_fk FOREIGN KEY (branch_uuid, tenant_uuid) REFERENCES branch (uuid, tenant_uuid); \
        \ALTER TABLE branch_data ADD CONSTRAINT branch_data_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE knowledge_model_migration ADD CONSTRAINT knowledge_model_migration_pk PRIMARY KEY (branch_uuid, tenant_uuid); \
        \ALTER TABLE knowledge_model_migration ADD CONSTRAINT knowledge_model_migration_branch_uuid_fk FOREIGN KEY (branch_uuid, tenant_uuid) REFERENCES branch (uuid, tenant_uuid); \
        \ALTER TABLE knowledge_model_migration ADD CONSTRAINT knowledge_model_migration_branch_previous_package_id_fk FOREIGN KEY (branch_previous_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid); \
        \ALTER TABLE knowledge_model_migration ADD CONSTRAINT knowledge_model_migration_target_package_id_fk FOREIGN KEY (target_package_id, tenant_uuid) REFERENCES package (id, tenant_uuid); \
        \ALTER TABLE knowledge_model_migration ADD CONSTRAINT knowledge_model_migration_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE audit ADD CONSTRAINT audit_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE audit ADD CONSTRAINT audit_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid); \
        \ALTER TABLE audit ADD CONSTRAINT audit_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid); \
        \ALTER TABLE action_key ADD CONSTRAINT action_key_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE action_key ADD CONSTRAINT action_key_identity_fk FOREIGN KEY (identity, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid) ON DELETE CASCADE; \
        \ALTER TABLE action_key ADD CONSTRAINT action_key_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid);"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
