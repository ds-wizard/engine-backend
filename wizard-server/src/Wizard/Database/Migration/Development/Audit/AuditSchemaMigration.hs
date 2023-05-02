module Wizard.Database.Migration.Development.Audit.AuditSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Audit) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Audit) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) drop tables"
  let sql = "drop table if exists audit cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) create table"
  let sql =
        "create table audit \
        \     ( \
        \         uuid            uuid not null \
        \             constraint audit_pk \
        \                 primary key, \
        \         component       varchar not null, \
        \         action          varchar not null, \
        \         entity          varchar not null, \
        \         body            json not null, \
        \         created_by      uuid, \
        \         app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \           constraint audit_app_uuid_fk \
        \             references app, \
        \         created_at      timestamp with time zone not null \
        \     ); \
        \ create unique index audit_uuid_uindex \
        \     on audit (uuid); \
        \  \
        \ alter table audit \
        \    add constraint audit_user_entity_uuid_fk \
        \       foreign key (created_by) references user_entity (uuid) on delete cascade;"
  let action conn = execute_ conn sql
  runDB action
