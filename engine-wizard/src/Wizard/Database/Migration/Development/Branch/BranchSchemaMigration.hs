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
  let sql =
        "drop table if exists branch_data cascade; \
        \drop table if exists branch cascade; "
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Branch) create table"
  let sql =
        "create table branch \
        \ ( \
        \     uuid uuid not null \
        \         constraint branch_pk \
        \             primary key, \
        \     name varchar not null, \
        \     km_id varchar not null, \
        \     previous_package_id varchar, \
        \     owner_uuid uuid not null \
        \         constraint branch_user_entity_uuid_fk \
        \             references user_entity, \
        \     created_at timestamptz not null, \
        \     updated_at timestamptz not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint branch_app_uuid_fk \
        \         references app \
        \); \
        \  \
        \ create unique index branch_uuid_uindex \
        \     on branch (uuid); \
        \ alter table branch \
        \    add constraint branch_package_id_fk \
        \        foreign key (previous_package_id, app_uuid) references package (id, app_uuid); \
        \   \
        \create table branch_data \
        \ ( \
        \     branch_uuid uuid not null \
        \         constraint branch_data_pk \
        \             primary key \
        \         constraint branch_data_branch_uuid_fk \
        \             references branch, \
        \     metamodel_version int not null, \
        \     events json, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint branch_app_uuid_fk \
        \         references app, \
        \     created_at timestamptz not null, \
        \     updated_at timestamptz not null \
        \); \
        \  \
        \create unique index branch_data_branch_uuid_uindex \
        \     on branch_data (branch_uuid); "
  let action conn = execute_ conn sql
  runDB action
