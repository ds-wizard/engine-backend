module Wizard.Database.Migration.Development.QuestionnaireAction.QuestionnaireActionSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireAction) drop tables"
  let sql = "DROP TABLE IF EXISTS questionnaire_action CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireAction) create table"
  let sql =
        "CREATE TABLE questionnaire_action \
        \( \
        \    id                varchar     NOT NULL, \
        \    name              varchar     NOT NULL, \
        \    organization_id   varchar     NOT NULL, \
        \    action_id       varchar     NOT NULL, \
        \    version           varchar     NOT NULL, \
        \    metamodel_version integer     NOT NULL, \
        \    description       varchar     NOT NULL, \
        \    readme            varchar     NOT NULL, \
        \    license           varchar     NOT NULL, \
        \    allowed_packages  jsonb       NOT NULL, \
        \    url               varchar, \
        \    config            jsonb       NOT NULL, \
        \    enabled           bool        NOT NULL, \
        \    tenant_uuid       uuid        NOT NULL, \
        \    created_at        timestamptz NOT NULL, \
        \    updated_at        timestamptz NOT NULL, \
        \    CONSTRAINT questionnaire_action_pk PRIMARY KEY (id, tenant_uuid), \
        \    CONSTRAINT questionnaire_action_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
