module Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Instance) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Instance) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Instance) drop tables"
  let sql = "DROP TABLE IF EXISTS instance_config_mail;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
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
