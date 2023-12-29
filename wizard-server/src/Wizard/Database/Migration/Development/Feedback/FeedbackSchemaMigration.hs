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
        \    uuid          uuid        NOT NULL, \
        \    issue_id      int         NOT NULL, \
        \    question_uuid uuid        NOT NULL, \
        \    package_id    varchar     NOT NULL, \
        \    title         varchar     NOT NULL, \
        \    content       varchar     NOT NULL, \
        \    created_at    timestamptz NOT NULL, \
        \    updated_at    timestamptz NOT NULL, \
        \    tenant_uuid   uuid        NOT NULL, \
        \    CONSTRAINT feedback_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT feedback_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES package (id, tenant_uuid), \
        \    CONSTRAINT feedback_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \); \
        \ \
        \CREATE INDEX feedback_package_id_index ON feedback (package_id, tenant_uuid); \
        \ \
        \CREATE INDEX feedback_question_uuid_index ON feedback (question_uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action
