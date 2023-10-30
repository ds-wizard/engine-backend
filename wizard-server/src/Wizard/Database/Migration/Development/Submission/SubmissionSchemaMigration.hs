module Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Submission) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Submission) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) drop tables"
  let sql = "DROP TABLE IF EXISTS submission CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) create table"
  let sql =
        "CREATE TABLE submission \
        \( \
        \    uuid          uuid        NOT NULL, \
        \    state         varchar     NOT NULL, \
        \    location      varchar, \
        \    returned_data varchar, \
        \    service_id    varchar     NOT NULL, \
        \    document_uuid uuid, \
        \    created_by    uuid, \
        \    created_at    timestamptz, \
        \    updated_at    timestamptz NOT NULL, \
        \    tenant_uuid   uuid        NOT NULL, \
        \    CONSTRAINT submission_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT submission_document_uuid_fk FOREIGN KEY (document_uuid, tenant_uuid) REFERENCES document (uuid, tenant_uuid), \
        \    CONSTRAINT submission_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \); \
        \ \
        \CREATE INDEX submission_document_uuid_index ON submission (document_uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action
