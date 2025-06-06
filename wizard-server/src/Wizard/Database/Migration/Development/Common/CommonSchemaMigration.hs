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
        "DROP FUNCTION IF EXISTS gravatar_hash; \
        \DROP FUNCTION IF EXISTS create_persistent_command_from_entity_id; \
        \DROP FUNCTION IF EXISTS create_persistent_command; \
        \DROP FUNCTION IF EXISTS is_outdated; \
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
  createPersistentCommandFunction
  createPersistentCommandFromEntityIdFunction
  createGravatarFunction

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

createPersistentCommandFunction = do
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
  runDB action

createPersistentCommandFromEntityIdFunction = do
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
  runDB action

createGravatarFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION gravatar_hash(email VARCHAR) RETURNS VARCHAR \
        \    language plpgsql \
        \as \
        \$$ \
        \DECLARE \
        \    hash VARCHAR; \
        \BEGIN \
        \    SELECT md5(lower(trim(email))) \
        \    INTO hash; \
        \    RETURN hash; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
