module Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger
import Shared.Database.DAO.CommonSql

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Organization) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Organization) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Organization) drop tables"
  let sql = "drop table if exists organization;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Organization) create tables"
  let sql =
        "create table organization \
            \ ( \
            \     organization_id varchar                  not null \
            \         constraint organization_pk \
            \             primary key, \
            \     name            varchar                  not null, \
            \     description     varchar                  not null, \
            \     email           varchar                  not null, \
            \     role            varchar                  not null, \
            \     token           varchar                  not null, \
            \     active          boolean                  not null, \
            \     logo            varchar, \
            \     created_at      timestamp with time zone not null, \
            \     updated_at      timestamp with time zone not null \
            \ ); \
            \create unique index organization_organization_id_uindex \
            \    on organization (organization_id); \
            \create unique index organization_token_uindex \
            \    on organization (token); "
  let action conn = execute_ conn sql
  runDB action
