module Wizard.Database.Migration.Production.Migration_0024_commonFn.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    {mmNumber = 24, mmName = "Common Functions", mmDescription = "Add common functions, package and template state"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createMajorVersionFn dbPool
  createMinorVersionFn dbPool
  createPatchVersionFn dbPool
  createCompareVersionFn dbPool
  createGetPackageStateFn dbPool
  createGetTemplateStateFn dbPool

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

createGetPackageStateFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_package_state(remote_version varchar, local_version varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN remote_version IS NULL THEN 'UnknownPackageState' \
        \               WHEN compare_version(remote_version, local_version) = 'LT' THEN 'UnpublishedPackageState' \
        \               WHEN compare_version(remote_version, local_version) = 'EQ' THEN 'UpToDatePackageState' \
        \               WHEN compare_version(remote_version, local_version) = 'GT' THEN 'OutdatedPackageState' \
        \               END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createGetTemplateStateFn dbPool = do
  let sql =
        "CREATE or REPLACE FUNCTION get_template_state(remote_version varchar, local_version varchar, actual_metamodel_version int, template_metamodel_version int) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    state varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN actual_metamodel_version != template_metamodel_version IS NULL THEN 'UnsupportedMetamodelVersionTemplateState' \
        \               WHEN remote_version IS NULL THEN 'UnknownTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'LT' THEN 'UnpublishedTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'EQ' THEN 'UpToDateTemplateState' \
        \               WHEN compare_version(remote_version, local_version) = 'GT' THEN 'OutdatedTemplateState' \
        \               END \
        \    INTO state; \
        \    RETURN state; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
