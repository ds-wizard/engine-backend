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
  let sql = "drop table if exists questionnaire_importer cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/QuestionnaireImporter) create table"
  let sql =
        "create table questionnaire_importer \
        \ ( \
        \     id                     varchar                  not null, \
        \     name                   varchar                  not null, \
        \     organization_id        varchar                  not null, \
        \     importer_id            varchar                  not null, \
        \     version                varchar                  not null, \
        \     metamodel_version      integer                  not null, \
        \     description            varchar                  not null, \
        \     readme                 varchar                  not null, \
        \     license                varchar                  not null, \
        \     allowed_packages       json                     not null, \
        \     url                    varchar, \
        \     enabled                bool                     not null, \
        \     tenant_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint questionnaire_importer_tenant_uuid_fk \
        \         references tenant, \
        \     created_at             timestamp with time zone not null, \
        \     updated_at             timestamp with time zone not null \
        \ ); \
        \ \
        \alter table questionnaire_importer \
        \    add constraint questionnaire_importer_pk primary key (id, tenant_uuid); \
        \create unique index questionnaire_importer_id_uindex \
        \     on questionnaire_importer (id, tenant_uuid); "
  let action conn = execute_ conn sql
  runDB action
