module Wizard.Database.Migration.Development.Limit.AppLimitSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Limit) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Limit) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Limit) drop table"
  let sql = "drop table if exists app_limit cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Limit) create table"
  let sql =
        "create table app_limit \
         \ ( \
         \     uuid              uuid              not null \
         \         constraint app_limit_pk \
         \             primary key, \
         \     users             integer,\
         \     active_users      integer,\
         \     knowledge_models  integer,\
         \     branches          integer,\
         \     templates         integer,\
         \     questionnaires    integer,\
         \     documents         integer,\
         \     storage           bigint,\
         \     created_at timestamp with time zone not null,\
         \     updated_at timestamp with time zone not null \
         \ ); \
         \  \
         \ create unique index app_limit_uuid_uindex \
         \     on app_limit (uuid);"
  let action conn = execute_ conn sql
  runDB action
