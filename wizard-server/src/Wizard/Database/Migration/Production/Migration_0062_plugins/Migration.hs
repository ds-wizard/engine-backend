module Wizard.Database.Migration.Production.Migration_0062_plugins.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 62, mmName = "Add user plugins", mmDescription = "Add user plugins feature"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createTenantPluginSettingsTable dbPool
  createUserPluginSettingsTable dbPool

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
        \    CONSTRAINT user_plugin_settings_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
