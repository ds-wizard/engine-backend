module Wizard.Database.Migration.Production.Migration_0048_jsonb.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 48, mmName = "Jsonb", mmDescription = "Change type from json to jsonb"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  changeColumnTypeFromJsonToJsonb dbPool
  replaceFunctionGetBranchStateWithJsonb dbPool

changeColumnTypeFromJsonToJsonb dbPool = do
  let sql =
        "ALTER TABLE audit ALTER COLUMN body TYPE jsonb USING body::jsonb; \
        \ALTER TABLE prefab ALTER COLUMN content TYPE jsonb USING content::jsonb; \
        \ALTER TABLE branch_data ALTER COLUMN events TYPE jsonb USING events::jsonb; \
        \ALTER TABLE document_template ALTER COLUMN allowed_packages TYPE jsonb USING allowed_packages::jsonb; \
        \ALTER TABLE document_template ALTER COLUMN formats TYPE jsonb USING formats::jsonb; \
        \ALTER TABLE knowledge_model_cache ALTER COLUMN knowledge_model TYPE jsonb USING knowledge_model::jsonb; \
        \ALTER TABLE knowledge_model_migration ALTER COLUMN migration_state TYPE jsonb USING migration_state::jsonb; \
        \ALTER TABLE knowledge_model_migration ALTER COLUMN branch_events TYPE jsonb USING branch_events::jsonb; \
        \ALTER TABLE knowledge_model_migration ALTER COLUMN target_package_events TYPE jsonb USING target_package_events::jsonb; \
        \ALTER TABLE knowledge_model_migration ALTER COLUMN result_events TYPE jsonb USING result_events::jsonb; \
        \ALTER TABLE knowledge_model_migration ALTER COLUMN current_knowledge_model TYPE jsonb USING current_knowledge_model::jsonb; \
        \ALTER TABLE questionnaire_migration ALTER COLUMN resolved_question_uuids TYPE jsonb USING resolved_question_uuids::jsonb; \
        \ALTER TABLE package ALTER COLUMN events TYPE jsonb USING events::jsonb; \
        \ALTER TABLE questionnaire ALTER COLUMN selected_question_tag_uuids TYPE jsonb USING selected_question_tag_uuids::jsonb; \
        \ALTER TABLE questionnaire ALTER COLUMN events TYPE jsonb USING events::jsonb; \
        \ALTER TABLE questionnaire ALTER COLUMN versions TYPE jsonb USING versions::jsonb; \
        \ALTER TABLE questionnaire_action ALTER COLUMN allowed_packages TYPE jsonb USING allowed_packages::jsonb; \
        \ALTER TABLE questionnaire_action ALTER COLUMN config TYPE jsonb USING config::jsonb; \
        \ALTER TABLE questionnaire_importer ALTER COLUMN allowed_packages TYPE jsonb USING allowed_packages::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN organization TYPE jsonb USING organization::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN authentication TYPE jsonb USING authentication::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN privacy_and_support TYPE jsonb USING privacy_and_support::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN dashboard_and_login_screen TYPE jsonb USING dashboard_and_login_screen::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN look_and_feel TYPE jsonb USING look_and_feel::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN registry TYPE jsonb USING registry::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN knowledge_model TYPE jsonb USING knowledge_model::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN questionnaire TYPE jsonb USING questionnaire::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN submission TYPE jsonb USING submission::jsonb; \
        \ALTER TABLE tenant_config ALTER COLUMN owl TYPE jsonb USING owl::jsonb; \
        \ALTER TABLE user_entity ALTER COLUMN sources TYPE jsonb USING sources::jsonb; \
        \ALTER TABLE user_entity ALTER COLUMN submissions_props TYPE jsonb USING submissions_props::jsonb;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

replaceFunctionGetBranchStateWithJsonb dbPool = do
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
        \               WHEN jsonb_array_length(branch_data.events) > 0 THEN 'BSEdited' \
        \               WHEN knowledge_model_migration.migration_state ->> 'stateType' IS NOT NULL AND \
        \                    knowledge_model_migration.migration_state ->> 'stateType' = 'CompletedState' THEN 'BSMigrated' \
        \               WHEN fork_of_package_id != get_newest_package_2(fork_of_package_id, tenant_uuid, ARRAY['ReleasedPackagePhase', 'DeprecatedPackagePhase']) THEN 'BSOutdated' \
        \               WHEN True THEN 'BSDefault' END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
