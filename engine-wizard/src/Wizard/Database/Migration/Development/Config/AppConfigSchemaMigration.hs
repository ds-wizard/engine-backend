module Wizard.Database.Migration.Development.Config.AppConfigSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/AppConfig) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/AppConfig) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/AppConfig) drop tables"
  let sql = "drop table if exists app_config;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/AppConfig) create tables"
  let sql =
        " create table app_config \
        \ ( \
        \     uuid                uuid                     not null, \
        \     organization        json                     not null, \
        \     authentication      json                     not null, \
        \     privacy_and_support json                     not null, \
        \     dashboard           json                     not null, \
        \     look_and_feel       json                     not null, \
        \     registry            json                     not null, \
        \     knowledge_model     json                     not null, \
        \     questionnaire       json                     not null, \
        \     template            json                     not null, \
        \     submission          json                     not null, \
        \     created_at          timestamp with time zone not null, \
        \     updated_at          timestamp with time zone not null \
        \ ); \
        \ create unique index app_config_uuid_uindex \
        \   on app_config (uuid); \
        \ alter table app_config \
        \   add constraint app_config_pk \
        \      primary key (uuid);"
  let action conn = execute_ conn sql
  runDB action
