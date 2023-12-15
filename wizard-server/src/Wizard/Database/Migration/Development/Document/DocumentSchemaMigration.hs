module Wizard.Database.Migration.Development.Document.DocumentSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Document) drop tables"
  let sql = "DROP TABLE IF EXISTS document CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Document) create table"
  let sql =
        "CREATE TABLE document \
        \( \
        \    uuid                       uuid        NOT NULL, \
        \    name                       varchar     NOT NULL, \
        \    state                      varchar     NOT NULL, \
        \    durability                 varchar     NOT NULL, \
        \    questionnaire_uuid         uuid        NOT NULL, \
        \    questionnaire_event_uuid   uuid, \
        \    questionnaire_replies_hash bigint      NOT NULL, \
        \    document_template_id       varchar     NOT NULL, \
        \    format_uuid                uuid        NOT NULL, \
        \    created_by                 uuid, \
        \    retrieved_at               timestamptz, \
        \    finished_at                timestamptz, \
        \    created_at                 timestamptz NOT NULL, \
        \    file_name                  varchar, \
        \    content_type               varchar, \
        \    worker_log                 varchar, \
        \    tenant_uuid                uuid        NOT NULL, \
        \    file_size                  bigint, \
        \    CONSTRAINT document_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT document_questionnaire_uuid_fk FOREIGN KEY (questionnaire_uuid, tenant_uuid) REFERENCES questionnaire (uuid, tenant_uuid), \
        \    CONSTRAINT document_document_template_id_fk FOREIGN KEY (document_template_id, tenant_uuid) REFERENCES document_template (id, tenant_uuid), \
        \    CONSTRAINT document_created_by_fk FOREIGN KEY (created_by, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT document_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
