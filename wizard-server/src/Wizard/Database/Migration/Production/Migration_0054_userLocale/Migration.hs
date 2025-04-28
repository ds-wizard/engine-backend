module Wizard.Database.Migration.Production.Migration_0054_userLocale.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 54, mmName = "Add user Locale", mmDescription = "Add support for saving locale to user entity"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addUserLocale dbPool
  createPersistentCommandFunction dbPool
  createPersistentCommandFromEntityIdFunction dbPool
  createTriggerOnAfterLocaleDelete dbPool
  createPersistentCommandFromEntityUuidFunction dbPool
  createTriggerOnAfterQuestionnaireFileDelete dbPool
  deleteAllLocalesExceptDefault dbPool
  addTildeToDefaultLocale dbPool

addUserLocale dbPool = do
  let sql =
        "ALTER TABLE user_entity ADD COLUMN locale VARCHAR; \
        \ALTER TABLE user_entity ADD CONSTRAINT user_entity_locale_fk FOREIGN KEY (locale, tenant_uuid) REFERENCES locale(id, tenant_uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createPersistentCommandFunction dbPool = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command(component varchar, function varchar, body jsonb, tenant_uuid uuid) RETURNS int AS \
        \$$ \
        \BEGIN \
        \    INSERT INTO persistent_command (uuid, \
        \                                    state, \
        \                                    component, \
        \                                    function, \
        \                                    body, \
        \                                    last_error_message, \
        \                                    attempts, \
        \                                    max_attempts, \
        \                                    tenant_uuid, \
        \                                    created_by, \
        \                                    created_at, \
        \                                    updated_at, \
        \                                    internal, \
        \                                    destination, \
        \                                    last_trace_uuid) \
        \    VALUES (gen_random_uuid(), \
        \            'NewPersistentCommandState', \
        \            component, \
        \            function, \
        \            body, \
        \            NULL, \
        \            0, \
        \            10, \
        \            tenant_uuid, \
        \            NULL, \
        \            now(), \
        \            now(), \
        \            true, \
        \            NULL, \
        \            NULL); \
        \    return 1; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createPersistentCommandFromEntityIdFunction dbPool = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_entity_id() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \DECLARE \
        \    component varchar; \
        \    function  varchar; \
        \BEGIN \
        \    component := TG_ARGV[0]; \
        \    function := TG_ARGV[1]; \
        \ \
        \    PERFORM create_persistent_command( \
        \            component, \
        \            function, \
        \            jsonb_build_object('id', OLD.id), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTriggerOnAfterLocaleDelete dbPool = do
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_locale_delete \
        \    AFTER DELETE \
        \    ON locale \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_entity_id('locale', 'deleteFromS3');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createPersistentCommandFromEntityUuidFunction dbPool = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_questionnaire_file_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM create_persistent_command( \
        \            'questionnaire_file', \
        \            'deleteFromS3', \
        \            jsonb_build_object('questionnaireUuid', OLD.questionnaire_uuid, 'fileUuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTriggerOnAfterQuestionnaireFileDelete dbPool = do
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_questionnaire_file_delete \
        \    AFTER DELETE \
        \    ON questionnaire_file \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_questionnaire_file_delete();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

deleteAllLocalesExceptDefault dbPool = do
  let sql = "DELETE FROM locale WHERE id != 'wizard:default:1.0.0';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addTildeToDefaultLocale dbPool = do
  let sql = "UPDATE locale SET id = '~:default:1.0.0', organization_id='~' WHERE id = 'wizard:default:1.0.0';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
