module Wizard.Database.Migration.Production.Migration_0051_branchState.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 51, mmName = "Branch State", mmDescription = "Fix branch get state"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  fixBranchGetStateFn dbPool
  addRepliesToBranchData dbPool
  removeDocumentQuestionnaireForeignKey dbPool
  addBranchUuidtoDraftData dbPool
  updateMetamodelVersionForDocumentTemplateEditor dbPool

fixBranchGetStateFn dbPool = do
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
        \               WHEN knowledge_model_migration.migration_state ->> 'stateType' IS NOT NULL AND \
        \                    knowledge_model_migration.migration_state ->> 'stateType' = 'CompletedState' THEN 'BSMigrated' \
        \               WHEN jsonb_array_length(branch_data.events) > 0 THEN 'BSEdited' \
        \               WHEN fork_of_package_id != get_newest_package_2(fork_of_package_id, tenant_uuid, ARRAY['ReleasedPackagePhase', 'DeprecatedPackagePhase']) THEN 'BSOutdated' \
        \               WHEN True THEN 'BSDefault' END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addRepliesToBranchData dbPool = do
  let sql = "ALTER TABLE branch_data ADD COLUMN replies json NOT NULL DEFAULT '{}';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

removeDocumentQuestionnaireForeignKey dbPool = do
  let sql = "ALTER TABLE document DROP CONSTRAINT document_questionnaire_uuid_fk;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addBranchUuidtoDraftData dbPool = do
  let sql = "ALTER TABLE document_template_draft_data ADD COLUMN branch_uuid uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

updateMetamodelVersionForDocumentTemplateEditor dbPool = do
  let sql = "UPDATE document_template SET metamodel_version = 16 WHERE phase = 'DraftDocumentTemplatePhase';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
