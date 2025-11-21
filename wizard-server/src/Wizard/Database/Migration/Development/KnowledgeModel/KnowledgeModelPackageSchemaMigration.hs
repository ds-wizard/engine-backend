module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Package) drop tables"
  let sql =
        "DROP TABLE IF EXISTS knowledge_model_package_event CASCADE; \
        \DROP TABLE IF EXISTS knowledge_model_package CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropFunctions :: AppContextM Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Package) drop functions"
  let sql =
        "DROP FUNCTION IF EXISTS get_newest_knowledge_model_package; \
        \DROP FUNCTION IF EXISTS get_newest_knowledge_model_package_2;\
        \DROP FUNCTION IF EXISTS get_organization_id;\
        \DROP FUNCTION IF EXISTS get_km_id;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createKnowledgeModelPackageTable
  createKnowledgeModelPackageEventTable

createKnowledgeModelPackageTable :: AppContextM Int64
createKnowledgeModelPackageTable = do
  logInfo _CMP_MIGRATION "(Table/Package) create table"
  let sql =
        "CREATE TABLE knowledge_model_package \
        \( \
        \    id                          varchar     NOT NULL, \
        \    name                        varchar     NOT NULL, \
        \    organization_id             varchar     NOT NULL, \
        \    km_id                       varchar     NOT NULL, \
        \    version                     varchar     NOT NULL, \
        \    metamodel_version           integer     NOT NULL, \
        \    description                 varchar     NOT NULL, \
        \    readme                      varchar     NOT NULL, \
        \    license                     varchar     NOT NULL, \
        \    previous_package_id         varchar, \
        \    fork_of_package_id          varchar, \
        \    merge_checkpoint_package_id varchar, \
        \    created_at                  timestamptz NOT NULL, \
        \    tenant_uuid                 uuid        NOT NULL, \
        \    phase                       varchar     NOT NULL, \
        \    non_editable                bool        NOT NULL, \
        \    CONSTRAINT knowledge_model_package_pk PRIMARY KEY (id, tenant_uuid), \
        \    CONSTRAINT knowledge_model_package_previous_package_id_fk FOREIGN KEY (previous_package_id, tenant_uuid) REFERENCES knowledge_model_package (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_package_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX knowledge_model_package_organization_id_km_id_index ON knowledge_model_package (organization_id, km_id, tenant_uuid); \
        \ \
        \CREATE INDEX knowledge_model_package_previous_package_id_index ON knowledge_model_package (previous_package_id, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action

createKnowledgeModelPackageEventTable :: AppContextM Int64
createKnowledgeModelPackageEventTable = do
  logInfo _CMP_MIGRATION "(Table/PackageEvent) create table"
  let sql =
        "CREATE TABLE IF NOT EXISTS knowledge_model_package_event \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    parent_uuid uuid        NOT NULL, \
        \    entity_uuid uuid        NOT NULL, \
        \    content     jsonb       NOT NULL, \
        \    package_id  varchar     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (uuid, package_id, tenant_uuid), \
        \    CONSTRAINT knowledge_model_package_event_package_id_fk FOREIGN KEY (package_id, tenant_uuid) REFERENCES knowledge_model_package (id, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_package_event_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: AppContextM Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Package) create functions"
  createGetNewestPackageFn
  createGetNewestPackage2Fn
  createGetOrganizationIdFn
  createGetKmIdFn

createGetNewestPackageFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_newest_knowledge_model_package(req_organization_id varchar, req_km_id varchar, req_tenant_uuid uuid, req_phase varchar[]) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CONCAT(organization_id, ':', km_id, ':', \
        \                  (max(string_to_array(version, '.')::int[]))[1] || \
        \                  '.' || \
        \                  (max(string_to_array(version, '.')::int[]))[2] || \
        \                  '.' || \
        \                  (max(string_to_array(version, '.')::int[]))[3]) \
        \    INTO p_id \
        \    FROM knowledge_model_package \
        \    WHERE organization_id = req_organization_id \
        \      AND km_id = req_km_id \
        \      AND tenant_uuid = req_tenant_uuid \
        \      AND phase = any(req_phase) \
        \    GROUP BY organization_id, km_id; \
        \    RETURN p_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetNewestPackage2Fn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_newest_knowledge_model_package_2(req_p_id varchar, req_tenant_uuid uuid, req_phase varchar[]) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    p_id varchar; \
        \BEGIN \
        \    SELECT CASE \
        \        WHEN req_p_id IS NULL THEN NULL \
        \        ELSE get_newest_knowledge_model_package(get_organization_id(req_p_id), get_km_id(req_p_id), req_tenant_uuid, req_phase) \
        \        END as newest_package_id \
        \    INTO p_id; \
        \    RETURN p_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetOrganizationIdFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_organization_id(req_p_id varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    organization_id varchar; \
        \BEGIN \
        \    SELECT split_part(req_p_id, ':', 1) \
        \    INTO organization_id; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action

createGetKmIdFn = do
  let sql =
        "CREATE or REPLACE FUNCTION get_km_id(req_p_id varchar) \
        \    RETURNS varchar \
        \    LANGUAGE plpgsql \
        \AS \
        \$$ \
        \DECLARE \
        \    km_id varchar; \
        \BEGIN \
        \    SELECT split_part(req_p_id, ':', 2) \
        \    INTO km_id; \
        \    RETURN km_id;\
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
