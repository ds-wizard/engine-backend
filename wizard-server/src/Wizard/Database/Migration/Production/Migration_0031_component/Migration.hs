module Wizard.Database.Migration.Production.Migration_0031_component.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 31, mmName = "Component", mmDescription = "Add component table"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createComponentTable dbPool
  addPhaseToPackage dbPool
  extendBranch dbPool
  changeReadmeInDefaultLocale dbPool
  createGetNewestPackageFn dbPool
  createGetNewestPackage2Fn dbPool

createComponentTable dbPool = do
  let sql =
        "CREATE TABLE component \
        \ ( \
        \     name                      varchar not null \
        \         constraint component_pk \
        \             primary key, \
        \     version                   varchar not null,\
        \     built_at timestamp with time zone not null,\
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null\
        \ ); \
        \  \
        \ CREATE UNIQUE INDEX component_name_uindex \
        \     ON component (name);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addPhaseToPackage dbPool = do
  let sql =
        "ALTER TABLE package \
        \   ADD phase varchar NOT NULL DEFAULT 'ReleasedPackagePhase';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

extendBranch dbPool = do
  let sql =
        "ALTER TABLE branch \
        \   ADD version varchar NOT NULL DEFAULT '1.0.0',\
        \   ADD description varchar NOT NULL DEFAULT 'Fill description here',\
        \   ADD readme varchar NOT NULL DEFAULT 'Fill readme here',\
        \   ADD license varchar NOT NULL DEFAULT 'Fill license here';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeReadmeInDefaultLocale dbPool = do
  let sql =
        "UPDATE locale \
        \SET description = 'Default English locale for Wizard UI', \
        \    readme      = concat('# Default English Locale for Wizard Client', \
        \              CHR(13), \
        \              CHR(10), \
        \              CHR(13), \
        \              CHR(10), \
        \              '[![Language](https://img.shields.io/badge/ISO%20639--1-en-blue)](https://en.wikipedia.org/wiki/English_language)', \
        \              CHR(13), \
        \              CHR(10), \
        \              CHR(13), \
        \              CHR(10), \
        \              'This is the default English locale embedded in the Wizard Client. Therefore, it is always complete and compatible with the version that it is shipped with.', \
        \              CHR(13), \
        \              CHR(10), \
        \              CHR(13), \
        \              CHR(10), \
        \              'The locale also cannot be exported or deleted. However, you can *Disable* it anytime as well as mark other locale to be used as *Default* if necessary.', \
        \              CHR(13), \
        \              CHR(10), \
        \              CHR(13), \
        \              CHR(10), \
        \              'In case you encounter any issues with this issue, please contact your service provider.', \
        \              CHR(13), \
        \              CHR(10) \
        \           ) \
        \WHERE id = 'wizard:default:1.0.0';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createGetNewestPackageFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_newest_package(req_organization_id varchar, req_km_id varchar, req_app_uuid uuid, req_phase varchar[]) \
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
        \      AND app_uuid = req_app_uuid \
        \      AND phase = any(req_phase) \
        \    GROUP BY organization_id, km_id; \
        \    RETURN p_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createGetNewestPackage2Fn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_newest_package_2(req_p_id varchar, req_app_uuid uuid, req_phase varchar[]) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \        WHEN req_p_id IS NULL THEN NULL \
        \        ELSE get_newest_package(get_organization_id(req_p_id), get_km_id(req_p_id), req_app_uuid, req_phase) \
        \        END as newest_package_id \
        \    INTO p_id; \
        \    RETURN p_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
