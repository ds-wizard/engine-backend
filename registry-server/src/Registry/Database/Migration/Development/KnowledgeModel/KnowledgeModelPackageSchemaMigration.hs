module Registry.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Registry.Database.DAO.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelPackage) drop tables"
  let sql =
        "DROP TABLE IF EXISTS knowledge_model_package_event CASCADE; \
        \DROP TABLE IF EXISTS knowledge_model_package CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createKnowledgeModelPackageTable
  createKnowledgeModelPackageEventTable

createKnowledgeModelPackageTable :: AppContextM Int64
createKnowledgeModelPackageTable = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelPackage) create table"
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
        \    CONSTRAINT knowledge_model_package_pk PRIMARY KEY (id) \
        \); \
        \ \
        \CREATE INDEX knowledge_model_package_organization_id_km_id_index ON knowledge_model_package (organization_id, km_id); \
        \ \
        \CREATE INDEX knowledge_model_package_previous_package_id_index ON knowledge_model_package (previous_package_id);"
  let action conn = execute_ conn sql
  runDB action

createKnowledgeModelPackageEventTable :: AppContextM Int64
createKnowledgeModelPackageEventTable = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelPackageEvent) create table"
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
        \    CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (uuid, package_id), \
        \    CONSTRAINT knowledge_model_package_event_package_id_fk FOREIGN KEY (package_id) REFERENCES knowledge_model_package (id) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
