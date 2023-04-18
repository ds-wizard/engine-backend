module Registry.Database.Migration.Development.Package.PackageSchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Package) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Package) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Package) drop tables"
  let sql = "drop table if exists package;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Package) create table"
  let sql =
        "create table package \
        \ ( \
        \     id                          varchar                  not null \
        \         constraint package_pk \
        \             primary key, \
        \     name                        varchar                  not null, \
        \     organization_id             varchar                  not null, \
        \     km_id                       varchar                  not null, \
        \     version                     varchar                  not null, \
        \     metamodel_version           integer                  not null, \
        \     description                 varchar                  not null, \
        \     readme                      varchar                  not null, \
        \     license                     varchar                  not null, \
        \     previous_package_id         varchar, \
        \     fork_of_package_id          varchar, \
        \     merge_checkpoint_package_id varchar, \
        \     events                      json                     not null, \
        \     created_at                  timestamp with time zone not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null, \
        \     phase                       varchar                  not null default 'ReleasedPackagePhase' \
        \ ); \
        \create unique index package_id_uindex \
        \     on package (id); \
        \create index package_organization_id_km_id_index \
        \     on package (organization_id, km_id); \
        \create index package_previous_package_id_index \
        \     on package (previous_package_id);"
  let action conn = execute_ conn sql
  runDB action
