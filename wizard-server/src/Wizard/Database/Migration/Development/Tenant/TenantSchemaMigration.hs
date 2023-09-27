module Wizard.Database.Migration.Development.Tenant.TenantSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Tenant) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Tenant) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Tenant) drop table"
  let sql =
        "DROP TABLE IF EXISTS tenant_limit_bundle; \
        \DROP TABLE IF EXISTS tenant_plan;\
        \DROP TABLE IF EXISTS tenant_config;\
        \DROP TABLE IF EXISTS tenant;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createTenantTable
  createTenantConfigTable
  createTenantPlanTable
  createTenantLimitBundleTable

createTenantTable = do
  logInfo _CMP_MIGRATION "(Table/Tenant) create table"
  let sql =
        "create table tenant \
        \ ( \
        \     uuid              uuid              not null \
        \         constraint tenant_pk \
        \             primary key, \
        \     tenant_id         varchar           not null, \
        \     name              varchar           not null, \
        \     server_domain     varchar           not null, \
        \     client_url        varchar           not null, \
        \     enabled           bool              not null, \
        \     created_at timestamp with time zone not null, \
        \     updated_at timestamp with time zone not null, \
        \     server_url        varchar           not null, \
        \     admin_server_url  varchar, \
        \     admin_client_url  varchar \
        \ ); \
        \  \
        \ create unique index tenant_uuid_uindex \
        \     on tenant (uuid);"
  let action conn = execute_ conn sql
  runDB action

createTenantConfigTable = do
  logInfo _CMP_MIGRATION "(Table/TenantConfig) create tables"
  let sql =
        "CREATE TABLE tenant_config \
        \ ( \
        \     uuid                        uuid                     not null, \
        \     organization                json                     not null, \
        \     authentication              json                     not null, \
        \     privacy_and_support         json                     not null, \
        \     dashboard_and_login_screen  json                     not null, \
        \     look_and_feel               json                     not null, \
        \     registry                    json                     not null, \
        \     knowledge_model             json                     not null, \
        \     questionnaire               json                     not null, \
        \     submission                  json                     not null, \
        \     created_at                  timestamp with time zone not null, \
        \     updated_at                  timestamp with time zone not null, \
        \     feature                     json                     not null, \
        \     owl                         json                     not null, \
        \     mail_config_uuid            uuid \
        \ ); \
        \ \
        \CREATE UNIQUE INDEX tenant_config_uuid_uindex \
        \   ON tenant_config (uuid); \
        \ \
        \ALTER TABLE tenant_config \
        \   ADD CONSTRAINT tenant_config_pk \
        \      PRIMARY KEY (uuid); \
        \ALTER TABLE tenant_config \
        \    ADD CONSTRAINT tenant_config_instance_config_mail_uuid_fk \
        \        FOREIGN KEY (mail_config_uuid) REFERENCES instance_config_mail (uuid);"
  let action conn = execute_ conn sql
  runDB action

createTenantPlanTable = do
  logInfo _CMP_MIGRATION "(Table/TenantPlan) create table"
  let sql =
        "create table tenant_plan \
        \ ( \
        \     uuid              uuid              not null \
        \         constraint tenant_plan_pk \
        \             primary key, \
        \     name              varchar not null, \
        \     users             integer, \
        \     since             timestamp with time zone, \
        \     until             timestamp with time zone, \
        \     test              bool not null, \
        \     tenant_uuid          uuid not null, \
        \     created_at        timestamp with time zone not null, \
        \     updated_at        timestamp with time zone not null \
        \ ); \
        \  \
        \ create unique index tenant_plan_uuid_uindex \
        \     on tenant_plan (uuid);"
  let action conn = execute_ conn sql
  runDB action

createTenantLimitBundleTable = do
  logInfo _CMP_MIGRATION "(Table/TenantLimitBundle) create table"
  let sql =
        "create table tenant_limit_bundle \
        \ ( \
        \     uuid                            uuid not null \
        \         constraint tenant_limit_bundle_pk \
        \             primary key, \
        \     users                           integer,\
        \     active_users                    integer,\
        \     knowledge_models                integer,\
        \     branches                        integer,\
        \     document_templates              integer,\
        \     questionnaires                  integer,\
        \     documents                       integer,\
        \     storage                         bigint,\
        \     created_at                      timestamp with time zone not null,\
        \     updated_at                      timestamp with time zone not null, \
        \     document_template_drafts        integer,\
        \     locales                         integer\
        \ ); \
        \  \
        \ create unique index tenant_limit_bundle_uuid_uindex \
        \     on tenant_limit_bundle (uuid);"
  let action conn = execute_ conn sql
  runDB action
