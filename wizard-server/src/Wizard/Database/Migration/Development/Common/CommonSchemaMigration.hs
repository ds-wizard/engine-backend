module Wizard.Database.Migration.Development.Common.CommonSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropFunctions :: AppContextM Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Common) drop functions"
  let sql =
        "DROP FUNCTION IF EXISTS is_outdated; \
        \DROP FUNCTION IF EXISTS major_version; \
        \DROP FUNCTION IF EXISTS minor_version;\
        \DROP FUNCTION IF EXISTS patch_version;\
        \DROP FUNCTION IF EXISTS compare_version;"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: AppContextM Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Common) create functions"
  createMajorVersionFn
  createMinorVersionFn
  createPatchVersionFn
  createCompareVersionFn
  createIsOutdatedVersionFn

createMajorVersionFn = do
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
  runDB action

createMinorVersionFn = do
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
  runDB action

createPatchVersionFn = do
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
  runDB action

createCompareVersionFn = do
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
  runDB action

createIsOutdatedVersionFn = do
  let sql =
        "CREATE or REPLACE FUNCTION is_outdated(version_1 varchar, version_2 varchar) \
        \    RETURNS bool \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    outdated varchar; \
        \BEGIN \
        \    SELECT CASE \
        \               WHEN compare_version(version_1, version_2) = 'GT' THEN true \
        \               ELSE false \
        \               END \
        \    INTO outdated; \
        \    RETURN outdated; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
