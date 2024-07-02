module Wizard.Database.Migration.Production.Migration_0046_tenantFeature.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 46, mmName = "Tenant Feature", mmDescription = "Remove tenant feature column"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  removeTenantConfigFeature dbPool
  addKnowledgeModelCache dbPool

removeTenantConfigFeature dbPool = do
  let sql = "ALTER TABLE tenant_config DROP feature"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addKnowledgeModelCache dbPool = do
  let sql =
        "CREATE TABLE knowledge_model_cache \
        \( \
        \    package_id      varchar     NOT NULL, \
        \    tag_uuids       text[]      NOT NULL, \
        \    knowledge_model json        NOT NULL, \
        \    tenant_uuid     uuid        NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_cache_pk PRIMARY KEY (package_id, tag_uuids, tenant_uuid), \
        \    CONSTRAINT knowledge_model_cache_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT knowledge_model_cache_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
