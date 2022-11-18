module Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Locale) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Locale) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) drop table"
  let sql = "drop table if exists locale cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) create table"
  let sql =
        "create table locale \
        \ ( \
        \     uuid              uuid              not null \
        \         constraint locale_pk \
        \             primary key, \
        \     name              varchar not null,\
        \     code          varchar not null,\
        \     fallback          bool not null,\
        \     enabled           bool not null,\
        \     app_uuid          uuid not null \
        \       constraint locale_app_uuid_fk \
        \         references app, \
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null \
        \ ); \
        \  \
        \ create unique index locale_uuid_uindex \
        \     on locale (uuid);"
  let action conn = execute_ conn sql
  runDB action
