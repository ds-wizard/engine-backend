module Wizard.Database.Migration.Production.Migration_0045_analytics.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 45, mmName = "Analytics", mmDescription = "Rename reporting to analytics"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE tenant RENAME reporting_server_url TO analytics_server_url; \
        \ALTER TABLE tenant RENAME reporting_client_url TO analytics_client_url; \
        \ \
        \UPDATE tenant \
        \SET analytics_server_url = replace(analytics_server_url, 'reporting', 'analytics'), \
        \    analytics_client_url = replace(analytics_client_url, 'reporting', 'analytics'); \
        \ \
        \UPDATE user_token \
        \SET name = 'Analytics'  \
        \WHERE type = 'AppKeyUserTokenType' AND name = 'Reporting'"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
