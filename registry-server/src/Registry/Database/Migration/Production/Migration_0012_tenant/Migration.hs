module Registry.Database.Migration.Production.Migration_0012_tenant.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 12, mmName = "Tenant", mmDescription = "Rename App to Tenant"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE action_key RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE document_template RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE document_template_asset RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE document_template_file RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE locale RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE package RENAME app_uuid TO tenant_uuid; \
        \ALTER TABLE persistent_command RENAME app_uuid TO tenant_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
