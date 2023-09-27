module Wizard.Database.Migration.Development.Acl.AclSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Acl/Group) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Acl/Group) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Acl/Group) drop tables"
  let sql = "drop table if exists acl_group cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Acl/Group) create table"
  let sql =
        " create table acl_group \
        \ ( \
        \   id varchar not null, \
        \   name varchar not null, \
        \   description varchar not null, \
        \   tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint acl_group_tenant_uuid_fk \
        \           references tenant \
        \ ); \
        \  \
        \ create unique index acl_group_id_uindex \
        \    on acl_group (id); \
        \  \
        \ alter table acl_group \
        \   add constraint acl_group_pk \
        \      primary key (id); "
  let action conn = execute_ conn sql
  runDB action
