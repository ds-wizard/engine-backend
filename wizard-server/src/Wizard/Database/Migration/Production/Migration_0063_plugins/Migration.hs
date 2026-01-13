module Wizard.Database.Migration.Production.Migration_0063_plugins.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 63, mmName = "Add user plugins", mmDescription = "Add user plugins feature"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createPluginTable dbPool
  createTenantPluginSettingsTable dbPool
  createUserPluginSettingsTable dbPool
  createPersistentCommandFromEntityUuidFunction dbPool
  changeLocalePrimaryKeyFromIdToUuid dbPool

createPluginTable dbPool = do
  let sql =
        "CREATE TABLE plugin \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    url          varchar     NOT NULL, \
        \    enabled      boolean     NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT plugin_pk PRIMARY KEY (uuid, tenant_uuid) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTenantPluginSettingsTable dbPool = do
  let sql =
        "CREATE TABLE tenant_plugin_settings \
        \( \
        \    tenant_uuid  uuid        NOT NULL, \
        \    plugin_uuid  uuid        NOT NULL, \
        \    values       jsonb       NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT tenant_plugin_settings_pk PRIMARY KEY (tenant_uuid, plugin_uuid), \
        \    CONSTRAINT tenant_plugin_settings_plugin_uuid_fk FOREIGN KEY (plugin_uuid, tenant_uuid) REFERENCES plugin (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT tenant_plugin_settings_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createUserPluginSettingsTable dbPool = do
  let sql =
        "CREATE TABLE user_plugin_settings \
        \( \
        \    user_uuid    uuid        NOT NULL, \
        \    plugin_uuid  uuid        NOT NULL, \
        \    values       jsonb       NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT user_plugin_settings_pk PRIMARY KEY (user_uuid, plugin_uuid), \
        \    CONSTRAINT user_plugin_settings_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_plugin_settings_plugin_uuid_fk FOREIGN KEY (plugin_uuid, tenant_uuid) REFERENCES plugin (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_plugin_settings_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createPersistentCommandFromEntityUuidFunction dbPool = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_entity_uuid() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \DECLARE \
        \    component varchar; \
        \    function  varchar; \
        \    destination  varchar; \
        \BEGIN \
        \    component := TG_ARGV[0]; \
        \    function := TG_ARGV[1]; \
        \    destination := TG_ARGV[2]; \
        \ \
        \    PERFORM create_persistent_command( \
        \            component, \
        \            function, \
        \            jsonb_build_object('uuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeLocalePrimaryKeyFromIdToUuid dbPool = do
  let sql =
        "ALTER TABLE user_entity DROP CONSTRAINT user_entity_locale_fk; \
        \ \
        \UPDATE locale SET organization_id = '~' WHERE id = '~:default:1.0.0'; \
        \ \
        \ALTER TABLE locale DROP CONSTRAINT locale_pk; \
        \ALTER TABLE locale RENAME COLUMN id TO uuid; \
        \ALTER TABLE locale ALTER COLUMN uuid TYPE uuid USING gen_random_uuid(); \
        \ALTER TABLE locale ADD CONSTRAINT locale_pk PRIMARY KEY (uuid); \
        \ \
        \UPDATE user_entity \
        \SET locale = locale.uuid \
        \FROM locale \
        \WHERE user_entity.locale = locale.organization_id || ':' || locale.locale_id || ':' || locale.version AND user_entity.tenant_uuid = locale.tenant_uuid; \
        \ \
        \ALTER TABLE user_entity ALTER COLUMN locale TYPE uuid USING locale::uuid; \
        \ALTER TABLE user_entity ADD CONSTRAINT user_entity_locale_fk FOREIGN KEY (locale) REFERENCES locale (uuid) ON DELETE SET NULL; \
        \ \
        \CREATE OR REPLACE TRIGGER trg_locale_after_delete_s3 \
        \    AFTER DELETE \
        \    ON locale \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_entity_uuid('locale', 'deleteFromS3'); \
        \ \
        \DROP TRIGGER trg_locale_before_delete_unset_user ON locale; \
        \ \
        \DROP FUNCTION unset_user_entity_locale;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
