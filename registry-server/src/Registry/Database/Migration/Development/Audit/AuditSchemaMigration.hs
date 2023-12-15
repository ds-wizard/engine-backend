module Registry.Database.Migration.Development.Audit.AuditSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) drop table"
  let sql = "DROP TABLE IF EXISTS audit;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) create table"
  let sql =
        "CREATE TABLE audit \
        \( \
        \    type                varchar     NOT NULL, \
        \    organization_id     varchar     NOT NULL, \
        \    instance_statistics json        NOT NULL, \
        \    package_id          varchar     NOT NULL, \
        \    created_at          timestamptz NOT NULL \
        \);"
  let action conn = execute_ conn sql
  runDB action
