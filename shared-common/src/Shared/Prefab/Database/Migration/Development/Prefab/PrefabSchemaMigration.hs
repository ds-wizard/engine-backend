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
  let sql = "drop table if exists prefab cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Prefab) create table"
  let sql =
        "create table prefab \
        \ ( \
        \     uuid              uuid              not null, \
        \     type              varchar           not null,\
        \     name              varchar           not null,\
        \     content           json              not null, \
        \     tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint prefab_tenant_uuid_fk \
        \         references tenant, \
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null, \
        \     constraint prefab_pk \
        \        primary key (uuid, tenant_uuid) \
        \ ); \
        \  \
        \ create unique index prefab_uuid_uindex \
        \     on prefab (uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action
