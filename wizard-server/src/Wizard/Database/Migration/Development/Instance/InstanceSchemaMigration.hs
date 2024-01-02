module Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Instance) drop tables"
  let sql = "DROP TABLE IF EXISTS instance_config_mail;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/InstanceConfigMail) create tables"
  let sql =
        "CREATE TABLE instance_config_mail \
        \( \
        \    uuid              uuid    NOT NULL, \
        \    enabled           boolean NOT NULL, \
        \    sender_name       text, \
        \    sender_email      text    NOT NULL, \
        \    host              text    NOT NULL, \
        \    port              integer, \
        \    security          text    NOT NULL, \
        \    username          text, \
        \    password          text, \
        \    rate_limit_window integer, \
        \    rate_limit_count  integer, \
        \    timeout           integer, \
        \    CONSTRAINT instance_config_mail_pk PRIMARY KEY (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
