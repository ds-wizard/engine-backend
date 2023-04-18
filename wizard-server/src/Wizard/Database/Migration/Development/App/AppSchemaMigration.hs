module Wizard.Database.Migration.Development.App.AppSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/App) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/App) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/App) drop table"
  let sql = "drop table if exists app cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/App) create table"
  let sql =
        "create table app \
        \ ( \
        \     uuid              uuid              not null \
        \         constraint app_pk \
        \             primary key, \
        \     app_id            varchar           not null,\
        \     name              varchar           not null,\
        \     server_domain     varchar           not null,\
        \     client_url        varchar           not null,\
        \     enabled           bool              not null,\
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null, \
        \     server_url        varchar           not null\
        \ ); \
        \  \
        \ create unique index app_uuid_uindex \
        \     on app (uuid);"
  let action conn = execute_ conn sql
  runDB action
