module Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Registry) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Registry) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Registry) drop tables"
  let sql =
        "drop table if exists registry_organization cascade; \
        \drop table if exists registry_package cascade; \
        \drop table if exists registry_template cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createOrganizationTable
  createPackageTable
  createTemplateTable

createOrganizationTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryOrganization) create table"
  let sql =
        "create table registry_organization \
        \ ( \
        \     organization_id varchar                  not null, \
        \     name            varchar                  not null, \
        \     logo            varchar, \
        \     created_at      timestamp with time zone not null \
        \ ); \
        \alter table registry_organization\
        \     add constraint registry_organization_pk primary key (organization_id);\
        \create unique index registry_organization_id_uindex \
        \     on registry_organization (organization_id); "
  let action conn = execute_ conn sql
  runDB action

createPackageTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryPackage) create table"
  let sql =
        "create table registry_package \
        \ ( \
        \     organization_id varchar                  not null, \
        \     km_id           varchar                  not null, \
        \     remote_version  varchar                  not null, \
        \     created_at      timestamp with time zone not null \
        \ ); \
        \alter table registry_package\
        \     add constraint registry_package_pk primary key (organization_id, km_id);\
        \create unique index registry_package_id_uindex \
        \     on registry_package (organization_id, km_id); "
  let action conn = execute_ conn sql
  runDB action

createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryPackage) create table"
  let sql =
        "create table registry_template \
        \ ( \
        \     organization_id varchar                  not null, \
        \     template_id     varchar                  not null, \
        \     remote_version  varchar                  not null, \
        \     created_at      timestamp with time zone not null \
        \ ); \
        \alter table registry_template\
        \     add constraint registry_template_pk primary key (organization_id, template_id);\
        \create unique index registry_template_id_uindex \
        \     on registry_template (organization_id, template_id); "
  let action conn = execute_ conn sql
  runDB action
