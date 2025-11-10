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
        \    type                           varchar     NOT NULL, \
        \    organization_id                varchar     NOT NULL, \
        \    created_at                     timestamptz NOT NULL, \
        \    user_count                     int, \
        \    knowledge_model_package_count  int, \
        \    knowledge_model_editor_count   int, \
        \    questionnaire_count            int, \
        \    document_template_count        int, \
        \    document_count                 int, \
        \    knowledge_model_package_id     varchar, \
        \    document_template_id           varchar, \
        \    locale_id                      varchar \
        \);"
  let action conn = execute_ conn sql
  runDB action
