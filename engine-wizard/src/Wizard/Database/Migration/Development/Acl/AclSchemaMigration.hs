module Wizard.Database.Migration.Development.Acl.AclSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

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
        \   description varchar not null \
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
