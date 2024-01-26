module Wizard.Database.Migration.Production.Migration_0042_isOutdated.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 42, mmName = "IsOutdated", mmDescription = "Add isOutdated function, remove get_*_state functions"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addIsOutdatedFunction dbPool
  dropGetPackageStateFunction dbPool
  dropGetTemplateStateFunction dbPool
  dropGetLocaleStateFunction dbPool
  addReporting dbPool

addIsOutdatedFunction dbPool = do
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
  liftIO $ withResource dbPool action
  return Nothing

dropGetPackageStateFunction dbPool = do
  let sql = "DROP FUNCTION get_package_state"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropGetTemplateStateFunction dbPool = do
  let sql = "DROP FUNCTION get_template_state"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropGetLocaleStateFunction dbPool = do
  let sql = "DROP FUNCTION get_locale_state"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addReporting dbPool = do
  let sql =
        "ALTER TABLE tenant ADD reporting_server_url VARCHAR; \
        \ALTER TABLE tenant ADD reporting_client_url VARCHAR; \
        \ \
        \UPDATE tenant \
        \SET reporting_server_url = replace(admin_server_url, 'admin', 'reporting'), \
        \    reporting_client_url = replace(admin_client_url, 'admin', 'reporting')"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
