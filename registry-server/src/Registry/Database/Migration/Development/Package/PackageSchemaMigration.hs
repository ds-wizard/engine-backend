module Registry.Database.Migration.Development.Package.PackageSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Package) drop tables"
  let sql = "DROP TABLE IF EXISTS package;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Package) create table"
  let sql =
        "CREATE TABLE package \
        \( \
        \    id                          varchar     NOT NULL, \
        \    name                        varchar     NOT NULL, \
        \    organization_id             varchar     NOT NULL, \
        \    km_id                       varchar     NOT NULL, \
        \    version                     varchar     NOT NULL, \
        \    metamodel_version           integer     NOT NULL, \
        \    description                 varchar     NOT NULL, \
        \    readme                      varchar     NOT NULL, \
        \    license                     varchar     NOT NULL, \
        \    previous_package_id         varchar, \
        \    fork_of_package_id          varchar, \
        \    merge_checkpoint_package_id varchar, \
        \    events                      json        NOT NULL, \
        \    created_at                  timestamptz NOT NULL, \
        \    tenant_uuid                 uuid        NOT NULL, \
        \    phase                       varchar     NOT NULL, \
        \    non_editable                bool        NOT NULL, \
        \    CONSTRAINT package_pk PRIMARY KEY (id) \
        \); \
        \ \
        \CREATE INDEX package_organization_id_km_id_index ON package (organization_id, km_id); \
        \ \
        \CREATE INDEX package_previous_package_id_index ON package (previous_package_id);"
  let action conn = execute_ conn sql
  runDB action
