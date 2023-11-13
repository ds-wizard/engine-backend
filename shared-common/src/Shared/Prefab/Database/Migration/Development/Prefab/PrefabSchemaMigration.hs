module Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Constant.Component
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger

runMigration :: AppContextC s sc m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Prefab) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Prefab) ended"

dropTables :: AppContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Prefab) drop table"
  let sql = "DROP TABLE IF EXISTS prefab CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Prefab) create table"
  let sql =
        "CREATE TABLE prefab \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    type        varchar     NOT NULL, \
        \    name        varchar     NOT NULL, \
        \    content     json        NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT prefab_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT prefab_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
