module Wizard.Database.Migration.Production.Migration_0050_kmValidation.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 50, mmName = "Add KM validation", mmDescription = "Add validation for questions in Knowledge Model"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  upgradeKmMetamodel dbPool
  updateTenantLimitBundleValues dbPool
  updateTenantLimitBundle dbPool
  dropTenantPlan dbPool

upgradeKmMetamodel dbPool = do
  let sql =
        "DELETE FROM knowledge_model_cache; \
        \UPDATE tenant SET state = 'PendingHousekeepingTenantState';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

updateTenantLimitBundleValues dbPool = do
  let sql =
        "UPDATE tenant_limit_bundle \
        \SET users= -30000, \
        \    active_users= -30000, \
        \    knowledge_models= -60000, \
        \    branches= -60000, \
        \    document_templates= -60000, \
        \    questionnaires= -60000, \
        \    documents= -180000, \
        \    storage= -300 * 5 * 1000 * 1000, \
        \    document_template_drafts= -60000, \
        \    locales= -60000 \
        \WHERE users IS NULL;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

updateTenantLimitBundle dbPool = do
  let sql =
        "ALTER TABLE tenant_limit_bundle ALTER COLUMN users SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN active_users SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN knowledge_models SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN branches SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN document_templates SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN questionnaires SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN documents SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN storage SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN document_template_drafts SET NOT NULL;\
        \ALTER TABLE tenant_limit_bundle ALTER COLUMN locales SET NOT NULL;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropTenantPlan dbPool = do
  let sql = "DROP TABLE tenant_plan"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
