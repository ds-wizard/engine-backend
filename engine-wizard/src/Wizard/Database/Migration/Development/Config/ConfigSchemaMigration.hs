module Wizard.Database.Migration.Development.Config.ConfigSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Config) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Config) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Config) drop tables"
  let sql =
        "DROP TABLE IF EXISTS app_config; \
        \DROP TABLE IF EXISTS instance_config_mail;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createInstanceConfigMailTable
  createAppConfigTable

createInstanceConfigMailTable = do
  logInfo _CMP_MIGRATION "(Table/InstanceConfigMail) create tables"
  let sql =
        "CREATE TABLE instance_config_mail \
        \( \
        \    uuid              uuid                 not null, \
        \    enabled           boolean default true not null, \
        \    sender_name       text, \
        \    sender_email      text                 not null, \
        \    host              text                 not null, \
        \    port              integer, \
        \    security          text                 not null, \
        \    username          text, \
        \    password          text, \
        \    rate_limit_window integer, \
        \    rate_limit_count  integer, \
        \    timeout           integer \
        \); \
        \ \
        \CREATE UNIQUE INDEX instance_config_mail_uuid_uindex \
        \   ON instance_config_mail (uuid); \
        \ \
        \ALTER TABLE instance_config_mail \
        \   ADD CONSTRAINT instance_config_mail_pk \
        \      PRIMARY KEY (uuid);"
  let action conn = execute_ conn sql
  runDB action

createAppConfigTable = do
  logInfo _CMP_MIGRATION "(Table/AppConfig) create tables"
  let sql =
        "CREATE TABLE app_config \
        \ ( \
        \     uuid                        uuid                     not null, \
        \     organization                json                     not null, \
        \     authentication              json                     not null, \
        \     privacy_and_support         json                     not null, \
        \     dashboard_and_login_screen  json                     not null, \
        \     look_and_feel               json                     not null, \
        \     registry                    json                     not null, \
        \     knowledge_model             json                     not null, \
        \     questionnaire               json                     not null, \
        \     submission                  json                     not null, \
        \     created_at                  timestamp with time zone not null, \
        \     updated_at                  timestamp with time zone not null, \
        \     feature                     json                     not null, \
        \     owl                         json                     not null, \
        \     mail_config_uuid            uuid \
        \ ); \
        \ \
        \CREATE UNIQUE INDEX app_config_uuid_uindex \
        \   ON app_config (uuid); \
        \ \
        \ALTER TABLE app_config \
        \   ADD CONSTRAINT app_config_pk \
        \      PRIMARY KEY (uuid); \
        \ALTER TABLE app_config \
        \    ADD CONSTRAINT app_config_instance_config_mail_uuid_fk \
        \        FOREIGN KEY (mail_config_uuid) REFERENCES instance_config_mail (uuid);"
  let action conn = execute_ conn sql
  runDB action
