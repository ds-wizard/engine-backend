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
    , mmDescription = "Introduce configurable per-tenant role entity with the new RolePermission catalog. Seed default Admin/Data Steward/Researcher roles per tenant, migrate user_entity.role from role name to role UUID (role_uuid), recompute user permissions to the new catalog (preserving internal DEV/TENANT permissions) into role_permissions, and point config_authentication.default_role_uuid to the seeded role UUID. Also add the tenant_module table and drop the legacy per-tenant module URL columns from tenant."
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
        \  ARRAY['LocalesManageRolePermission','UsersManageRolePermission','SettingsManageRolePermission','ProjectTemplatesManageRolePermission','ProjectsViewRolePermission','ProjectsCommentRolePermission','ProjectsEditRolePermission','ProjectsManageRolePermission','KnowledgeModelEditorsUseRolePermission','KnowledgeModelsManageRolePermission','DocumentTemplateEditorsUseRolePermission','DocumentTemplatesManageRolePermission']::varchar[], \
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
