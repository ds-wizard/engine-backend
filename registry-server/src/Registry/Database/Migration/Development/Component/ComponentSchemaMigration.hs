module Registry.Database.Migration.Development.Component.ComponentSchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Component) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Component) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Component) drop table"
  let sql = "drop table if exists component cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Component) create table"
  let sql =
        "create table component \
        \ ( \
        \     name                      varchar not null \
        \         constraint component_pk \
        \             primary key, \
        \     version                   varchar not null,\
        \     built_at timestamp with time zone not null,\
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null\
        \ ); \
        \  \
        \ create unique index component_name_uindex \
        \     on component (name);"
  let action conn = execute_ conn sql
  runDB action
