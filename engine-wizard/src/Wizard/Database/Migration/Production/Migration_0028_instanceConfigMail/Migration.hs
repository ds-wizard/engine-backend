module Wizard.Database.Migration.Production.Migration_0028_instanceConfigMail.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 28, mmName = "Instance Config Mail", mmDescription = "Add config for mail to DB"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createInstanceConfigMailTable dbPool
  addMailConfigUuidToAppConfig dbPool

createInstanceConfigMailTable dbPool = do
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
  liftIO $ withResource dbPool action
  return Nothing

addMailConfigUuidToAppConfig dbPool = do
  let sql =
        "ALTER TABLE app_config \
        \     ADD mail_config_uuid uuid; \
        \ALTER TABLE app_config \
        \    ADD CONSTRAINT app_config_instance_config_mail_uuid_fk \
        \        FOREIGN KEY (mail_config_uuid) REFERENCES instance_config_mail (uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
