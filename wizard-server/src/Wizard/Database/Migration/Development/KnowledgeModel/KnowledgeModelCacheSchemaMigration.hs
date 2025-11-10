module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelCacheSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelCache) drop tables"
  let sql = "DROP TABLE IF EXISTS knowledge_model_cache CASCADE; "
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelCache) create tables"
  let sql =
        "CREATE TABLE knowledge_model_cache \
        \( \
        \    package_id varchar     NOT NULL, \
        \    tag_uuids                  text[]      NOT NULL, \
        \    knowledge_model            jsonb       NOT NULL, \
        \    tenant_uuid                uuid        NOT NULL, \
        \    created_at                 timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_cache_pk PRIMARY KEY (package_id, tag_uuids, tenant_uuid), \
        \    CONSTRAINT knowledge_model_cache_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES knowledge_model_package (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_cache_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
