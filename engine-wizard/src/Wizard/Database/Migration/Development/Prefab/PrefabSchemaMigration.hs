module Wizard.Database.Migration.Development.Prefab.PrefabSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Prefab) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Prefab) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Prefab) drop table"
  let sql = "drop table if exists prefab cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Prefab) create table"
  let sql =
        "create table prefab \
        \ ( \
        \     uuid              uuid              not null, \
        \     type              varchar           not null,\
        \     name              varchar           not null,\
        \     content           json              not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint prefab_app_uuid_fk \
        \         references app, \
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null, \
        \     constraint prefab_pk \
        \        primary key (uuid, app_uuid) \
        \ ); \
        \  \
        \ create unique index prefab_uuid_uindex \
        \     on prefab (uuid, app_uuid);"
  let action conn = execute_ conn sql
  runDB action
