module Registry.Database.Migration.Development.Audit.AuditSchemaMigration where

import Database.PostgreSQL.Simple

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Audit) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Audit) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) drop table"
  let sql = "drop table if exists audit;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) create table"
  let sql =
        "create table audit \
        \ ( \
        \     type                varchar                  not null, \
        \     organization_id     varchar                  not null, \
        \     instance_statistics json                     not null, \
        \     package_id          varchar                  not null, \
        \     created_at          timestamp with time zone not null \
        \ );"
  let action conn = execute_ conn sql
  runDB action
