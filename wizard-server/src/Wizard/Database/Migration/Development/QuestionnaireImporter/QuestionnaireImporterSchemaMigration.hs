module Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireImporter) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/QuestionnaireImporter) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireImporter) drop tables"
  let sql = "DROP TABLE IF EXISTS questionnaire_importer CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireImporter) create table"
  let sql =
        "CREATE TABLE questionnaire_importer \
        \( \
        \    id                varchar     NOT NULL, \
        \    name              varchar     NOT NULL, \
        \    organization_id   varchar     NOT NULL, \
        \    importer_id       varchar     NOT NULL, \
        \    version           varchar     NOT NULL, \
        \    metamodel_version integer     NOT NULL, \
        \    description       varchar     NOT NULL, \
        \    readme            varchar     NOT NULL, \
        \    license           varchar     NOT NULL, \
        \    allowed_packages  json        NOT NULL, \
        \    url               varchar, \
        \    enabled           bool        NOT NULL, \
        \    tenant_uuid       uuid        NOT NULL, \
        \    created_at        timestamptz NOT NULL, \
        \    updated_at        timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_importer_pk PRIMARY KEY (id, tenant_uuid), \
        \    CONSTRAINT questionnaire_importer_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
