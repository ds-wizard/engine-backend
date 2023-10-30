module Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Organization) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Organization) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Organization) drop tables"
  let sql = "DROP TABLE IF EXISTS organization;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Organization) create tables"
  let sql =
        "CREATE TABLE organization \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    name            varchar     NOT NULL, \
        \    description     varchar     NOT NULL, \
        \    email           varchar     NOT NULL, \
        \    role            varchar     NOT NULL, \
        \    token           varchar     NOT NULL, \
        \    active          boolean     NOT NULL, \
        \    logo            varchar, \
        \    created_at      timestamptz NOT NULL, \
        \    updated_at      timestamptz NOT NULL, \
        \    CONSTRAINT organization_pk PRIMARY KEY (organization_id) \
        \); \
        \ \
        \CREATE UNIQUE INDEX organization_token_uindex ON organization (token);"
  let action conn = execute_ conn sql
  runDB action
