module Wizard.Database.Migration.Development.Plan.AppPlanSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Plan) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Plan) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Plan) drop table"
  let sql = "drop table if exists app_plan cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Plan) create table"
  let sql =
        "create table app_plan \
        \ ( \
        \     uuid              uuid              not null \
        \         constraint app_plan_pk \
        \             primary key, \
        \     name              varchar not null, \
        \     users             integer, \
        \     since             timestamp with time zone, \
        \     until             timestamp with time zone, \
        \     test              bool not null, \
        \     app_uuid          uuid not null, \
        \     created_at        timestamp with time zone not null, \
        \     updated_at        timestamp with time zone not null \
        \ ); \
        \  \
        \ create unique index app_plan_uuid_uindex \
        \     on app_plan (uuid);"
  let action conn = execute_ conn sql
  runDB action
