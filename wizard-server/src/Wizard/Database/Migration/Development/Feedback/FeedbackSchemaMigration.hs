module Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Feedback) drop tables"
  let sql = "DROP TABLE IF EXISTS feedback CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Feedback) create table"
  let sql =
        "CREATE TABLE feedback \
        \( \
        \    uuid                       uuid        NOT NULL, \
        \    issue_id                   int         NOT NULL, \
        \    question_uuid              uuid        NOT NULL, \
        \    knowledge_model_package_id varchar     NOT NULL, \
        \    title                      varchar     NOT NULL, \
        \    content                    varchar     NOT NULL, \
        \    created_at                 timestamptz NOT NULL, \
        \    updated_at                 timestamptz NOT NULL, \
        \    tenant_uuid                uuid        NOT NULL, \
        \    CONSTRAINT feedback_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT feedback_knowledge_model_package_id_fk FOREIGN KEY (knowledge_model_package_id, tenant_uuid) REFERENCES knowledge_model_package (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT feedback_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX feedback_knowledge_model_package_id_id_index ON feedback (knowledge_model_package_id, tenant_uuid); \
        \ \
        \CREATE INDEX feedback_question_uuid_index ON feedback (question_uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action
