module Wizard.Database.Migration.Development.Branch.BranchSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Branch) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Branch) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Branch) drop tables"
  let sql = "drop table if exists branch cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Branch) create table"
  let sql =
        " create table branch \
        \ ( \
        \     uuid uuid not null \
        \         constraint branch_pk \
        \             primary key, \
        \     name varchar not null, \
        \     km_id varchar not null, \
        \     metamodel_version int not null, \
        \     previous_package_id varchar \
        \         constraint branch_package_id_fk \
        \             references package, \
        \     events json not null, \
        \     owner_uuid uuid not null \
        \         constraint branch_user_entity_uuid_fk \
        \             references user_entity, \
        \     created_at timestamptz not null, \
        \     updated_at timestamptz not null \
        \ ); \
        \  \
        \ create unique index branch_uuid_uindex \
        \     on branch (uuid); "
  let action conn = execute_ conn sql
  runDB action
