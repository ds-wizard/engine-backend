module Registry.Database.Migration.Production.Migration_0005_locale.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 5
    , mmName = "Locale"
    , mmDescription = "Add locale"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createMajorVersionFn dbPool
  createMinorVersionFn dbPool
  createPatchVersionFn dbPool
  createCompareVersionFn dbPool
  createLocaleTable dbPool

createMajorVersionFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION major_version(version varchar) \
        \    RETURNS int \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    major_version int; \
        \BEGIN \
        \    SELECT (string_to_array(version, '.')::int[])[1] \
        \    INTO major_version; \
        \    RETURN major_version; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createMinorVersionFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION minor_version(version varchar) \
        \    RETURNS int \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    minor_version int; \
        \BEGIN \
        \    SELECT (string_to_array(version, '.')::int[])[2] \
        \    INTO minor_version; \
        \    RETURN minor_version; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createPatchVersionFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION patch_version(version varchar) \
        \    RETURNS int \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    patch_version int; \
        \BEGIN \
        \    SELECT (string_to_array(version, '.')::int[])[3] \
        \    INTO patch_version; \
        \    RETURN patch_version; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createCompareVersionFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION compare_version(version_1 varchar, version_2 varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    version_order varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN major_version(version_1) = major_version(version_2) \
        \                   THEN CASE \
        \                            WHEN minor_version(version_1) = minor_version(version_2) \
        \                                THEN CASE \
        \                                         WHEN patch_version(version_1) = patch_version(version_2) THEN 'EQ' \
        \                                         WHEN patch_version(version_1) < patch_version(version_2) THEN 'LT' \
        \                                         WHEN patch_version(version_1) > patch_version(version_2) THEN 'GT' \
        \                                END \
        \                            WHEN minor_version(version_1) < minor_version(version_2) THEN 'LT' \
        \                            WHEN minor_version(version_1) > minor_version(version_2) THEN 'GT' \
        \                   END \
        \               WHEN major_version(version_1) < major_version(version_2) THEN 'LT' \
        \               WHEN major_version(version_1) > major_version(version_2) THEN 'GT' \
        \               END \
        \    INTO version_order; \
        \    RETURN version_order; \
        \END; \
        \$$;"
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
        \     app_uuid                  uuid default '00000000-0000-0000-0000-000000000000' not null,\
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null\
        \ ); \
        \  \
        \ CREATE UNIQUE INDEX locale_uuid_uindex \
        \     ON locale (id);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
