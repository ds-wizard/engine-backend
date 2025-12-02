module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelSecret) drop tables"
  let sql = "DROP TABLE IF EXISTS knowledge_model_secret CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelSecret) create table"
  let sql =
        "CREATE TABLE knowledge_model_secret \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    name        varchar     NOT NULL, \
        \    value       varchar     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_secret_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT knowledge_model_secret_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
