module Wizard.Database.Migration.Production.Migration_0065_idToUuid.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 65, mmName = "Switch from ID to UUID", mmDescription = "Switch from ID to UUID for knowledge models and document templates"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  updateUserPermissions dbPool
  dropProjectActionTable dbPool
  dropProjectImporterTable dbPool
  changeDocumentTemplatePrimaryKeyFromIdToUuid dbPool
  recreatePersistentCommandFromDocumentTemplateAssetDeleteFunction dbPool
  changeKnowledgeModelPrimaryKeyFromIdToUuid dbPool
  recreateGetNewestPackageFn dbPool
  recreateGetNewestPackageCoordinateFn dbPool
  recreateGetKnowledgeModelEditorForkOfPackageIdFn dbPool
  recreateGetKnowledgeModelEditorStateFn dbPool

updateUserPermissions dbPool = do
  let sql = "UPDATE user_entity SET permissions = array_remove(array_remove(permissions, 'PRJ_ACTION_PERM'), 'PRJ_IMPORTER_PERM');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropProjectActionTable dbPool = do
  let sql = "DROP TABLE project_action;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropProjectImporterTable dbPool = do
  let sql = "DROP TABLE project_importer;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeDocumentTemplatePrimaryKeyFromIdToUuid dbPool = do
  let sql =
        "ALTER TABLE config_submission_service_supported_format DROP CONSTRAINT config_submission_service_supported_format_dt_id_fk; \
        \ALTER TABLE config_submission_service_supported_format DROP CONSTRAINT config_submission_service_supported_format_format_uuid_fk; \
        \ALTER TABLE document DROP CONSTRAINT document_document_template_id_fk; \
        \ALTER TABLE document DROP CONSTRAINT document_format_uuid_fk; \
        \ALTER TABLE project DROP CONSTRAINT project_document_template_id_fk; \
        \ALTER TABLE project DROP CONSTRAINT questionnaire_format_uuid_fk; \
        \ALTER TABLE document_template_asset DROP CONSTRAINT document_template_asset_document_template_id_fk; \
        \ALTER TABLE document_template_draft_data DROP CONSTRAINT document_template_draft_data_document_template_id_fk; \
        \ALTER TABLE document_template_file DROP CONSTRAINT document_template_file_document_template_id_fk; \
        \ALTER TABLE document_template_format DROP CONSTRAINT document_template_format_document_template_id_fk; \
        \ALTER TABLE document_template_format_step DROP CONSTRAINT document_template_format_step_document_template_id_fk; \
        \ALTER TABLE document_template_format_step DROP CONSTRAINT document_template_format_step_format_uuid_fk; \
        \ \
        \ALTER TABLE document_template DROP CONSTRAINT document_template_pk; \
        \ALTER TABLE document_template RENAME COLUMN id TO uuid; \
        \ALTER TABLE document_template ALTER COLUMN uuid TYPE uuid USING gen_random_uuid(); \
        \ALTER TABLE document_template ADD CONSTRAINT document_template_pk PRIMARY KEY (uuid); \
        \ \
        \ALTER TABLE config_submission_service_supported_format RENAME document_template_id TO document_template_uuid; \
        \UPDATE config_submission_service_supported_format SET document_template_uuid = document_template.uuid FROM document_template WHERE config_submission_service_supported_format.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND config_submission_service_supported_format.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE config_submission_service_supported_format ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE config_submission_service_supported_format ADD CONSTRAINT config_submission_service_supported_format_dt_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document RENAME document_template_id TO document_template_uuid; \
        \UPDATE document SET document_template_uuid = document_template.uuid FROM document_template WHERE document.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document ADD CONSTRAINT document_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE project RENAME document_template_id TO document_template_uuid; \
        \UPDATE project SET document_template_uuid = document_template.uuid FROM document_template WHERE project.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND project.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE project ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE project ADD CONSTRAINT project_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE SET NULL; \
        \ \
        \ALTER TABLE document_template_asset RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_asset SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_asset.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_asset.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_asset ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_asset ADD CONSTRAINT document_template_asset_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document_template_draft_data RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_draft_data SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_draft_data.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_draft_data.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_draft_data ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_draft_data ADD CONSTRAINT document_template_draft_data_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document_template_file RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_file SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_file.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_file.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_file ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_file ADD CONSTRAINT document_template_file_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document_template_format RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_format SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_format.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_format.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_format ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_format ADD CONSTRAINT document_template_format_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE document_template_format_step RENAME document_template_id TO document_template_uuid; \
        \UPDATE document_template_format_step SET document_template_uuid = document_template.uuid FROM document_template WHERE document_template_format_step.document_template_uuid = document_template.organization_id || ':' || document_template.template_id || ':' || document_template.version AND document_template_format_step.tenant_uuid = document_template.tenant_uuid; \
        \ALTER TABLE document_template_format_step ALTER COLUMN document_template_uuid TYPE uuid USING document_template_uuid::uuid; \
        \ALTER TABLE document_template_format_step ADD CONSTRAINT document_template_format_step_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE; \
        \ \
        \ \
        \ALTER TABLE document_template_format DROP CONSTRAINT document_template_format_pk; \
        \ALTER TABLE document_template_format ADD CONSTRAINT document_template_format_pk PRIMARY KEY (document_template_uuid, uuid); \
        \ \
        \ALTER TABLE config_submission_service_supported_format ADD CONSTRAINT config_submission_service_supported_format_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE CASCADE; \
        \ALTER TABLE project ADD CONSTRAINT project_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE SET NULL; \
        \ALTER TABLE document ADD CONSTRAINT document_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE CASCADE; \
        \ALTER TABLE document_template_format_step ADD CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE CASCADE;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

recreatePersistentCommandFromDocumentTemplateAssetDeleteFunction dbPool = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_document_template_asset_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM create_persistent_command( \
        \            'document_template_asset', \
        \            'deleteFromS3', \
        \            jsonb_build_object('documentTemplateUuid', OLD.document_template_uuid, 'assetUuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeKnowledgeModelPrimaryKeyFromIdToUuid dbPool = do
  let sql =
        "DROP FUNCTION create_persistent_command_from_entity_id(); \
        \ \
        \DROP TABLE config_knowledge_model_public_package_pattern; \
        \ALTER TABLE config_knowledge_model DROP COLUMN public_enabled; \
        \ \
        \ \
        \ALTER TABLE feedback DROP CONSTRAINT feedback_knowledge_model_package_id_fk; \
        \DROP INDEX feedback_knowledge_model_package_id_index; \
        \ \
        \ALTER TABLE knowledge_model_migration DROP CONSTRAINT knowledge_model_migration_editor_previous_package_id_fk; \
        \ALTER TABLE knowledge_model_migration DROP CONSTRAINT knowledge_model_migration_target_package_id_fk; \
        \ \
        \ALTER TABLE knowledge_model_cache DROP CONSTRAINT knowledge_model_cache_package_id_fk; \
        \ALTER TABLE knowledge_model_cache DROP CONSTRAINT knowledge_model_cache_pk; \
        \ \
        \ALTER TABLE project DROP CONSTRAINT project_knowledge_model_package_id_fk; \
        \ \
        \ALTER TABLE knowledge_model_editor DROP CONSTRAINT knowledge_model_editor_previous_package_id_fk; \
        \ \
        \ALTER TABLE knowledge_model_package_event DROP CONSTRAINT knowledge_model_package_event_package_id_fk; \
        \ALTER TABLE knowledge_model_package_event DROP CONSTRAINT knowledge_model_package_event_pk; \
        \ \
        \ALTER TABLE knowledge_model_package DROP CONSTRAINT knowledge_model_package_previous_package_id_fk; \
        \ALTER TABLE knowledge_model_package DROP CONSTRAINT knowledge_model_package_pk; \
        \ \
        \ \
        \ALTER TABLE config_owl RENAME COLUMN previous_package_id TO previous_package_uuid; \
        \ \
        \ALTER TABLE feedback RENAME COLUMN knowledge_model_package_id TO knowledge_model_package_uuid; \
        \ \
        \ALTER TABLE knowledge_model_migration RENAME COLUMN editor_previous_package_id TO editor_previous_package_uuid; \
        \ALTER TABLE knowledge_model_migration RENAME COLUMN target_package_id TO target_package_uuid; \
        \ \
        \ALTER TABLE knowledge_model_cache RENAME COLUMN package_id TO package_uuid; \
        \ \
        \ALTER TABLE project RENAME COLUMN knowledge_model_package_id TO knowledge_model_package_uuid; \
        \ \
        \ALTER TABLE knowledge_model_editor RENAME COLUMN previous_package_id TO previous_package_uuid; \
        \ \
        \ALTER TABLE knowledge_model_package_event RENAME COLUMN package_id TO package_uuid; \
        \ \
        \ALTER TABLE knowledge_model_package RENAME COLUMN previous_package_id TO previous_package_uuid; \
        \ALTER TABLE knowledge_model_package RENAME COLUMN id TO uuid; \
        \ \
        \ \
        \ALTER TABLE knowledge_model_package ALTER COLUMN uuid TYPE uuid USING gen_random_uuid(); \
        \ALTER TABLE knowledge_model_package ADD CONSTRAINT knowledge_model_package_pk PRIMARY KEY (uuid); \
        \ \
        \UPDATE knowledge_model_package SET previous_package_uuid = previous_pkg.uuid FROM knowledge_model_package previous_pkg WHERE knowledge_model_package.previous_package_uuid = previous_pkg.organization_id || ':' || previous_pkg.km_id || ':' || previous_pkg.version AND knowledge_model_package.tenant_uuid = previous_pkg.tenant_uuid; \
        \ALTER TABLE knowledge_model_package ALTER COLUMN previous_package_uuid TYPE uuid USING previous_package_uuid::uuid; \
        \ALTER TABLE knowledge_model_package ADD CONSTRAINT knowledge_model_package_previous_package_uuid_fk FOREIGN KEY (previous_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE ; \
        \ \
        \UPDATE knowledge_model_package_event SET package_uuid = knowledge_model_package.uuid FROM knowledge_model_package WHERE knowledge_model_package_event.package_uuid = knowledge_model_package.organization_id || ':' || knowledge_model_package.km_id || ':' || knowledge_model_package.version AND knowledge_model_package_event.tenant_uuid = knowledge_model_package.tenant_uuid; \
        \ALTER TABLE knowledge_model_package_event ALTER COLUMN package_uuid TYPE uuid USING package_uuid::uuid; \
        \ALTER TABLE knowledge_model_package_event ADD CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (package_uuid, uuid); \
        \ALTER TABLE knowledge_model_package_event ADD CONSTRAINT knowledge_model_package_event_package_uuid_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE ; \
        \ \
        \UPDATE knowledge_model_editor SET previous_package_uuid = knowledge_model_package.uuid FROM knowledge_model_package WHERE knowledge_model_editor.previous_package_uuid = knowledge_model_package.organization_id || ':' || knowledge_model_package.km_id || ':' || knowledge_model_package.version AND knowledge_model_editor.tenant_uuid = knowledge_model_package.tenant_uuid; \
        \ALTER TABLE knowledge_model_editor ALTER COLUMN previous_package_uuid TYPE uuid USING previous_package_uuid::uuid; \
        \ALTER TABLE knowledge_model_editor ADD CONSTRAINT knowledge_model_editor_previous_package_uuid_fk FOREIGN KEY (previous_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE ; \
        \ \
        \UPDATE project SET knowledge_model_package_uuid = knowledge_model_package.uuid FROM knowledge_model_package WHERE project.knowledge_model_package_uuid = knowledge_model_package.organization_id || ':' || knowledge_model_package.km_id || ':' || knowledge_model_package.version AND project.tenant_uuid = knowledge_model_package.tenant_uuid; \
        \ALTER TABLE project ALTER COLUMN knowledge_model_package_uuid TYPE uuid USING knowledge_model_package_uuid::uuid; \
        \ALTER TABLE project ADD CONSTRAINT project_knowledge_model_package_uuid_fk FOREIGN KEY (knowledge_model_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE ; \
        \ \
        \TRUNCATE knowledge_model_cache; \
        \ALTER TABLE knowledge_model_cache ALTER COLUMN package_uuid TYPE uuid USING package_uuid::uuid; \
        \ALTER TABLE knowledge_model_cache ADD CONSTRAINT knowledge_model_cache_pk PRIMARY KEY (package_uuid, tag_uuids); \
        \ALTER TABLE knowledge_model_cache ADD CONSTRAINT knowledge_model_cache_package_uuid_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE ; \
        \ \
        \TRUNCATE knowledge_model_migration; \
        \ALTER TABLE knowledge_model_migration ALTER COLUMN editor_previous_package_uuid TYPE uuid USING editor_previous_package_uuid::uuid; \
        \ALTER TABLE knowledge_model_migration ADD CONSTRAINT knowledge_model_migration_editor_previous_package_uuid_fk FOREIGN KEY (editor_previous_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE ; \
        \ \
        \ALTER TABLE knowledge_model_migration ALTER COLUMN target_package_uuid TYPE uuid USING target_package_uuid::uuid; \
        \ALTER TABLE knowledge_model_migration ADD CONSTRAINT knowledge_model_migration_target_package_uuid_fk FOREIGN KEY (target_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE ; \
        \ \
        \UPDATE feedback SET knowledge_model_package_uuid = knowledge_model_package.uuid FROM knowledge_model_package WHERE feedback.knowledge_model_package_uuid = knowledge_model_package.organization_id || ':' || knowledge_model_package.km_id || ':' || knowledge_model_package.version AND feedback.tenant_uuid = knowledge_model_package.tenant_uuid; \
        \ALTER TABLE feedback ALTER COLUMN knowledge_model_package_uuid TYPE uuid USING knowledge_model_package_uuid::uuid; \
        \ALTER TABLE feedback ADD CONSTRAINT feedback_knowledge_model_package_uuid_fk FOREIGN KEY (knowledge_model_package_uuid) REFERENCES knowledge_model_package (uuid) ON DELETE CASCADE; \
        \ \
        \ALTER TABLE config_owl ALTER COLUMN previous_package_uuid TYPE uuid USING previous_package_uuid::uuid; \
        \ \
        \ALTER TABLE knowledge_model_package ADD COLUMN public boolean NOT NULL DEFAULT FALSE; \
        \ \
        \ALTER TABLE knowledge_model_package ADD CONSTRAINT knowledge_model_package_coordinate_unique UNIQUE (organization_id, km_id, version, tenant_uuid)"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

recreateGetNewestPackageFn dbPool = do
  let sql =
        "DROP FUNCTION get_newest_knowledge_model_package(req_organization_id varchar, req_km_id varchar, req_tenant_uuid uuid, req_phase varchar[]); \
        \CREATE OR REPLACE FUNCTION get_newest_knowledge_model_package(req_organization_id varchar, req_km_id varchar, req_tenant_uuid uuid, req_phase varchar[]) \
        \    RETURNS uuid \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_uuid uuid; \
        \BEGIN \
        \    SELECT uuid \
        \    INTO p_uuid \
        \    FROM knowledge_model_package \
        \    WHERE organization_id = req_organization_id \
        \      AND km_id = req_km_id \
        \      AND tenant_uuid = req_tenant_uuid \
        \      AND phase = ANY (req_phase) \
        \    ORDER BY (string_to_array(version, '.')::int[])[1] DESC, \
        \             (string_to_array(version, '.')::int[])[2] DESC, \
        \             (string_to_array(version, '.')::int[])[3] DESC \
        \    LIMIT 1; \
        \ \
        \    RETURN p_uuid; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

recreateGetNewestPackageCoordinateFn dbPool = do
  let sql =
        "DROP FUNCTION get_newest_knowledge_model_package_2(req_p_i varchar, req_tenant_uuid uuid, req_phase varchar[]); \
        \CREATE or REPLACE FUNCTION get_newest_knowledge_model_package_coordinate(req_coordinate varchar, req_tenant_uuid uuid, req_phase varchar[]) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    target_uuid       uuid; \
        \    result_coordinate varchar; \
        \BEGIN \
        \    IF req_coordinate IS NULL THEN \
        \        RETURN NULL; \
        \    END IF; \
        \ \
        \    target_uuid := get_newest_knowledge_model_package( \
        \            get_organization_id(req_coordinate), \
        \            get_km_id(req_coordinate), \
        \            req_tenant_uuid, \
        \            req_phase \
        \                   ); \
        \ \
        \    IF target_uuid IS NOT NULL THEN \
        \        SELECT concat(organization_id, ':', km_id, ':', version) \
        \        INTO result_coordinate \
        \        FROM knowledge_model_package \
        \        WHERE uuid = target_uuid; \
        \    END IF; \
        \ \
        \    RETURN result_coordinate; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

recreateGetKnowledgeModelEditorForkOfPackageIdFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_knowledge_model_editor_fork_of_package_id(config_organization config_organization, \
        \                                                                         previous_pkg knowledge_model_package, \
        \                                                                         knowledge_model_editor knowledge_model_editor) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    fork_of_package_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN knowledge_model_editor.previous_package_uuid IS NULL THEN NULL \
        \               WHEN previous_pkg.organization_id = config_organization.organization_id AND \
        \                    previous_pkg.km_id = knowledge_model_editor.km_id THEN previous_pkg.fork_of_package_id \
        \               WHEN True THEN concat(previous_pkg.organization_id, ':', previous_pkg.km_id, ':', previous_pkg.version) END as fork_of_package_id \
        \    INTO fork_of_package_id; \
        \    RETURN fork_of_package_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

recreateGetKnowledgeModelEditorStateFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_knowledge_model_editor_state(editor knowledge_model_editor, \
        \                                                            knowledge_model_migration knowledge_model_migration, \
        \                                                            fork_of_package_id varchar, \
        \                                                            editor_tenant_uuid uuid) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND \
        \                    knowledge_model_migration.state ->> 'type' != 'CompletedKnowledgeModelMigrationState' THEN 'MigratingKnowledgeModelEditorState' \
        \               WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND \
        \                    knowledge_model_migration.state ->> 'type' = 'CompletedKnowledgeModelMigrationState' THEN 'MigratedKnowledgeModelEditorState' \
        \               WHEN (SELECT COUNT(*) FROM knowledge_model_editor_event editor_event WHERE editor_event.tenant_uuid = editor.tenant_uuid AND editor_event.editor_uuid = editor.uuid) > 0 THEN 'EditedKnowledgeModelEditorState' \
        \               WHEN fork_of_package_id != get_newest_knowledge_model_package_coordinate(fork_of_package_id, editor.tenant_uuid, ARRAY['ReleasedKnowledgeModelPackagePhase', 'DeprecatedKnowledgeModelPackagePhase']) THEN 'OutdatedKnowledgeModelEditorState' \
        \               WHEN True THEN 'DefaultKnowledgeModelEditorState' END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
