module Wizard.Database.Migration.Development.Level.LevelSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Level) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Level) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Level) drop tables"
  let sql = "drop table if exists level;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Level) create table"
  let sql =
        "create table level \
        \ ( \
        \   level int not null \
        \      constraint level_pk \
        \           primary key, \
        \   title varchar not null, \
        \   description varchar, \
        \   created_at timestamptz not null, \
        \   updated_at timestamptz not null \
        \ ); "
  let action conn = execute_ conn sql
  runDB action
