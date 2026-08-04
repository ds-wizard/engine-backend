module Wizard.Database.Migration.Production.Migration_0068_roles.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 68
    , mmName = "User Roles and Tenant Modules"
    , mmDescription = "Introduce configurable per-tenant role entity with the new RolePermission catalog. Seed default Admin/Data Steward/Researcher roles per tenant, migrate user_entity.role from role name to role UUID (role_uuid), recompute user permissions to the new catalog (preserving internal DEV/TENANT permissions) into role_permissions, and point config_authentication.default_role_uuid to the seeded role UUID. Also add the tenant_module table and drop the legacy per-tenant module URL columns from tenant. Add knowledge model translations (language columns and knowledge model locales). Normalize user_entity emails to lowercase (email and email_pending) and add a CHECK constraint enforcing lowercase email."
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createRoleTable dbPool
  seedRoles dbPool
  migrateRoleAndPermissions dbPool
  migrateRoleName dbPool
  migrateDefaultRole dbPool
  finalizeRoleColumns dbPool
  createTenantModuleTable dbPool
  dropTenantModuleUrlColumns dbPool
  createOpenIdClientSessionTable dbPool
  addKnowledgeModelLanguageColumns dbPool
  createKnowledgeModelLocaleTable dbPool
  lowercaseUserEmails dbPool

createRoleTable dbPool = do
  let sql =
        "CREATE TABLE role \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    name         varchar     NOT NULL, \
        \    permissions  varchar[]   NOT NULL, \
        \    is_admin     boolean     NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT role_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT role_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

seedRoles dbPool = do
  let sql =
        "INSERT INTO role (uuid, name, permissions, is_admin, tenant_uuid, created_at, updated_at) \
        \SELECT gen_random_uuid(), 'Admin', \
        \  ARRAY['UsersManageRolePermission','SettingsManageRolePermission','ProjectTemplatesManageRolePermission','ProjectsViewRolePermission','ProjectsCommentRolePermission','ProjectsEditRolePermission','ProjectsManageRolePermission','KnowledgeModelEditorsUseRolePermission','KnowledgeModelsManageRolePermission','DocumentTemplateEditorsUseRolePermission','DocumentTemplatesManageRolePermission']::varchar[], \
        \  true, t.uuid, now(), now() FROM tenant t; \
        \INSERT INTO role (uuid, name, permissions, is_admin, tenant_uuid, created_at, updated_at) \
        \SELECT gen_random_uuid(), 'Data Steward', \
        \  ARRAY['ProjectTemplatesManageRolePermission','KnowledgeModelEditorsUseRolePermission','KnowledgeModelsManageRolePermission','DocumentTemplateEditorsUseRolePermission','DocumentTemplatesManageRolePermission']::varchar[], \
        \  false, t.uuid, now(), now() FROM tenant t; \
        \INSERT INTO role (uuid, name, permissions, is_admin, tenant_uuid, created_at, updated_at) \
        \SELECT gen_random_uuid(), 'Researcher', \
        \  ARRAY[]::varchar[], \
        \  false, t.uuid, now(), now() FROM tenant t;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

migrateRoleAndPermissions dbPool = do
  let sql =
        "UPDATE user_entity u SET permissions = r.permissions || COALESCE(ARRAY(SELECT CASE p WHEN 'DEV_PERM' THEN 'DevUseRolePermission' WHEN 'TENANT_PERM' THEN 'TenantsManageRolePermission' END FROM unnest(u.permissions) AS p WHERE p IN ('DEV_PERM', 'TENANT_PERM')), ARRAY[]::varchar[]) \
        \FROM role r \
        \WHERE r.tenant_uuid = u.tenant_uuid AND ( \
        \  (u.role = 'admin' AND r.name = 'Admin') OR \
        \  (u.role = 'dataSteward' AND r.name = 'Data Steward') OR \
        \  (u.role = 'researcher' AND r.name = 'Researcher')); \
        \UPDATE user_entity u SET role = r.uuid::text \
        \FROM role r \
        \WHERE r.tenant_uuid = u.tenant_uuid AND ( \
        \  (u.role = 'admin' AND r.name = 'Admin') OR \
        \  (u.role = 'dataSteward' AND r.name = 'Data Steward') OR \
        \  (u.role = 'researcher' AND r.name = 'Researcher'));"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

migrateRoleName dbPool = do
  let sql =
        "ALTER TABLE user_entity ADD COLUMN role_name varchar NOT NULL DEFAULT ''; \
        \UPDATE user_entity u SET role_name = r.name FROM role r WHERE r.uuid::text = u.role; \
        \ALTER TABLE user_entity ALTER COLUMN role_name DROP DEFAULT;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

migrateDefaultRole dbPool = do
  let sql =
        "ALTER TABLE config_authentication RENAME COLUMN default_role TO default_role_uuid; \
        \UPDATE config_authentication a SET default_role_uuid = r.uuid::text \
        \FROM role r \
        \WHERE r.tenant_uuid = a.tenant_uuid AND ( \
        \  (a.default_role_uuid = 'admin' AND r.name = 'Admin') OR \
        \  (a.default_role_uuid = 'dataSteward' AND r.name = 'Data Steward') OR \
        \  (a.default_role_uuid = 'researcher' AND r.name = 'Researcher'));"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

finalizeRoleColumns dbPool = do
  let sql =
        "ALTER TABLE user_entity ALTER COLUMN role TYPE uuid USING role::uuid; \
        \ALTER TABLE config_authentication ALTER COLUMN default_role_uuid TYPE uuid USING default_role_uuid::uuid; \
        \ALTER TABLE user_entity RENAME COLUMN role TO role_uuid; \
        \ALTER TABLE user_entity ADD CONSTRAINT user_entity_role_uuid_fk FOREIGN KEY (role_uuid) REFERENCES role (uuid); \
        \ALTER TABLE user_entity RENAME COLUMN permissions TO role_permissions;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTenantModuleTable dbPool = do
  let sql =
        "CREATE TABLE tenant_module \
        \( \
        \    tenant_uuid         uuid        NOT NULL, \
        \    position            int         NOT NULL, \
        \    module_key          varchar     NOT NULL, \
        \    title               varchar     NOT NULL, \
        \    description         varchar     NOT NULL, \
        \    icon                varchar     NOT NULL, \
        \    url                 varchar     NOT NULL, \
        \    external            bool        NOT NULL, \
        \    required_permission varchar, \
        \    enabled             bool        NOT NULL DEFAULT true, \
        \    created_at          timestamptz NOT NULL, \
        \    updated_at          timestamptz NOT NULL, \
        \    CONSTRAINT tenant_module_pk PRIMARY KEY (tenant_uuid, position), \
        \    CONSTRAINT tenant_module_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropTenantModuleUrlColumns dbPool = do
  let sql =
        "ALTER TABLE tenant DROP COLUMN admin_server_url; \
        \ALTER TABLE tenant DROP COLUMN admin_client_url; \
        \ALTER TABLE tenant DROP COLUMN integration_hub_server_url; \
        \ALTER TABLE tenant DROP COLUMN integration_hub_client_url; \
        \ALTER TABLE tenant DROP COLUMN analytics_server_url; \
        \ALTER TABLE tenant DROP COLUMN analytics_client_url;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createOpenIdClientSessionTable dbPool = do
  let sql =
        "CREATE TABLE openid_client_session \
        \( \
        \    state       varchar     NOT NULL, \
        \    nonce       varchar     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    CONSTRAINT openid_client_session_pk PRIMARY KEY (state), \
        \    CONSTRAINT openid_client_session_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addKnowledgeModelLanguageColumns dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_package ADD COLUMN language varchar NOT NULL DEFAULT 'en'; \
        \ALTER TABLE knowledge_model_editor ADD COLUMN language varchar NOT NULL DEFAULT 'en'; \
        \ALTER TABLE project ADD COLUMN language varchar;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createKnowledgeModelLocaleTable dbPool = do
  let sql =
        "CREATE TABLE knowledge_model_locale \
        \( \
        \    uuid                         uuid        NOT NULL, \
        \    name                         varchar     NOT NULL, \
        \    code                         varchar     NOT NULL, \
        \    knowledge_model_package_uuid uuid        NOT NULL, \
        \    tenant_uuid                  uuid        NOT NULL, \
        \    created_at                   timestamptz NOT NULL, \
        \    updated_at                   timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_locale_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT knowledge_model_locale_package_uuid_fk FOREIGN KEY (knowledge_model_package_uuid) REFERENCES knowledge_model_package (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_locale_code_unique UNIQUE (knowledge_model_package_uuid, code, tenant_uuid) \
        \); \
        \CREATE OR REPLACE TRIGGER trg_knowledge_model_locale_after_delete_s3 \
        \    AFTER DELETE \
        \    ON knowledge_model_locale \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_entity_uuid('knowledge_model_locale', 'deleteFromS3');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

lowercaseUserEmails dbPool = do
  let sql =
        "UPDATE user_entity SET email = lower(email) WHERE email <> lower(email); \
        \UPDATE user_entity SET email_pending = lower(email_pending) WHERE email_pending IS NOT NULL AND email_pending <> lower(email_pending); \
        \ALTER TABLE user_entity ADD CONSTRAINT user_email_lowercase_check CHECK (email = lower(email));"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
