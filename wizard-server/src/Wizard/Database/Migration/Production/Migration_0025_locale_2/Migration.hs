module Wizard.Database.Migration.Production.Migration_0025_locale_2.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 25, mmName = "Locale 2", mmDescription = "Add locale management"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  dropLocaleTable dbPool
  createLocaleTable dbPool
  insertDefaultLocale dbPool
  createRegistryLocaleTable dbPool
  addLocPerm dbPool
  createGetLocaleStateFn dbPool

dropLocaleTable dbPool = do
  let sql = "DROP TABLE locale;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createLocaleTable dbPool = do
  let sql =
        "CREATE TABLE locale \
        \ ( \
        \     id                        varchar not null \
        \         CONSTRAINT locale_pk \
        \             PRIMARY KEY, \
        \     name                      varchar not null,\
        \     description               varchar not null,\
        \     code                      varchar not null,\
        \     organization_id           varchar not null,\
        \     locale_id                 varchar not null,\
        \     version                   varchar not null,\
        \     default_locale            bool not null,\
        \     license                   varchar not null,\
        \     readme                    varchar not null,\
        \     recommended_app_version   varchar not null,\
        \     enabled                   bool not null,\
        \     app_uuid                  uuid not null \
        \       CONSTRAINT locale_app_uuid_fk \
        \         REFERENCES app, \
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null\
        \ ); \
        \  \
        \ CREATE UNIQUE INDEX locale_uuid_uindex \
        \     ON locale (id);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertDefaultLocale dbPool = do
  let sql =
        "INSERT INTO locale (id, name, description, code, organization_id, locale_id, version, default_locale, license, readme, recommended_app_version, enabled, app_uuid, created_at, updated_at) VALUES ('wizard:default:1.0.0', 'English', 'Default english locale', 'en', 'wizard', 'default', '1.0.0', true, 'Apache-2.0', '# English Locale \
        \ \
        \This is a default english locale that is shipped together with the wizard itself.', '3.18.0', true, '00000000-0000-0000-0000-000000000000', '2022-01-21 00:00:00.000000 +00:00', '2022-01-21 00:00:00.000000 +00:00');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createRegistryLocaleTable dbPool = do
  let sql =
        "CREATE TABLE registry_locale \
        \ ( \
        \     organization_id varchar                  not null, \
        \     locale_id           varchar              not null, \
        \     remote_version  varchar                  not null, \
        \     created_at      timestamp with time zone not null \
        \ ); \
        \ALTER TABLE registry_locale\
        \     ADD CONSTRAINT registry_locale_pk PRIMARY KEY (organization_id, locale_id);\
        \CREATE UNIQUE INDEX registry_locale_id_uindex \
        \     ON registry_locale (organization_id, locale_id); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addLocPerm dbPool = do
  let sql = "UPDATE user_entity set permissions = permissions || '{LOC_PERM}' WHERE role = 'admin'"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createGetLocaleStateFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_locale_state(remote_version varchar, local_version varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN remote_version IS NULL THEN 'UnknownLocaleState' \
        \               WHEN compare_version(remote_version, local_version) = 'LT' THEN 'UnpublishedLocaleState' \
        \               WHEN compare_version(remote_version, local_version) = 'EQ' THEN 'UpToDateLocaleState' \
        \               WHEN compare_version(remote_version, local_version) = 'GT' THEN 'OutdatedLocaleState' \
        \               END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
