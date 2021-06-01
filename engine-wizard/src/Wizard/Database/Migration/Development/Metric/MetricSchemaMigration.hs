module Wizard.Database.Migration.Development.Metric.MetricSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Metric) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Metric) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Metric) drop tables"
  let sql = "drop table if exists metric;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Metric) create table"
  let sql =
        " create table metric \
        \ ( \
        \     uuid uuid not null, \
        \     title varchar not null, \
        \     abbreviation varchar, \
        \     description varchar, \
        \     reference_json json not null, \
        \     created_at timestamptz not null, \
        \     updated_at timestamptz not null \
        \ ); \
        \  \
        \ create unique index metric_uuid_uindex \
        \     on metric (uuid); \
        \  \
        \ alter table metric \
        \     add constraint metric_pk \
        \         primary key (uuid); "
  let action conn = execute_ conn sql
  runDB action
