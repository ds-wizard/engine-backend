module Wizard.Database.Migration.Production.Migration_0067_openIdClient.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 67, mmName = "OpenID Client", mmDescription = "Promote OpenID configuration to a first-class entity with UUID PK, scope flags and registrationEnabled. Add user_openid_identity link table, user_registration_pending table, and email verification columns on user_entity; drop legacy config_authentication_openid + user_entity.sources. Add per-tenant session and user email link expiration to config_authentication."}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createOpenIdClientTable dbPool
  prepareLegacyMapping dbPool
  copyOpenIdClientData dbPool
  createUserOpenIdIdentityTable dbPool
  createUserRegistrationPendingTable dbPool
  addUserEmailVerification dbPool
  dropLegacyOpenIdConfig dbPool
  addNonAdminLoginEnabled dbPool
  addConfigMailCustomTemplates dbPool
  addAuthExpirationColumns dbPool
  renameUserEmailLinkTable dbPool

createOpenIdClientTable dbPool = do
  let sql =
        "CREATE TABLE openid_client \
        \( \
        \    uuid                 uuid        NOT NULL, \
        \    name                 varchar     NOT NULL, \
        \    url                  varchar     NOT NULL, \
        \    client_id            varchar     NOT NULL, \
        \    client_secret        varchar     NOT NULL, \
        \    parameters           jsonb       NOT NULL, \
        \    style                jsonb       NOT NULL, \
        \    tenant_uuid          uuid        NOT NULL, \
        \    created_at           timestamptz NOT NULL, \
        \    updated_at           timestamptz NOT NULL, \
        \    registration_enabled bool        NOT NULL, \
        \    scope_profile        bool        NOT NULL, \
        \    scope_email          bool        NOT NULL, \
        \    CONSTRAINT openid_client_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT openid_client_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

prepareLegacyMapping dbPool = do
  let sql =
        "ALTER TABLE config_authentication_openid ADD COLUMN _new_uuid uuid; \
        \UPDATE config_authentication_openid SET _new_uuid = gen_random_uuid();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

copyOpenIdClientData dbPool = do
  let sql =
        "INSERT INTO openid_client \
        \    (uuid, name, url, client_id, client_secret, parameters, style, tenant_uuid, created_at, updated_at, registration_enabled, scope_profile, scope_email) \
        \SELECT \
        \    _new_uuid, \
        \    name, \
        \    url, \
        \    client_id, \
        \    client_secret, \
        \    parameters, \
        \    jsonb_build_object('icon', style_icon, 'background', style_background, 'color', style_color), \
        \    tenant_uuid, \
        \    created_at, \
        \    updated_at, \
        \    true, \
        \    true, \
        \    true \
        \FROM config_authentication_openid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createUserOpenIdIdentityTable dbPool = do
  let sql =
        "CREATE TABLE user_openid_identity \
        \( \
        \    uuid           uuid        NOT NULL, \
        \    external_id    varchar     NOT NULL, \
        \    external_label varchar, \
        \    user_uuid      uuid        NOT NULL, \
        \    provider_uuid  uuid        NOT NULL, \
        \    tenant_uuid    uuid        NOT NULL, \
        \    created_at     timestamptz NOT NULL, \
        \    CONSTRAINT user_openid_identity_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT user_openid_identity_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_openid_identity_provider_uuid_fk FOREIGN KEY (provider_uuid) REFERENCES openid_client (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_openid_identity_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE UNIQUE INDEX user_openid_identity_uindex ON user_openid_identity (external_id, provider_uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createUserRegistrationPendingTable dbPool = do
  let sql =
        "CREATE TABLE user_registration_pending \
        \( \
        \    uuid           uuid        NOT NULL, \
        \    hash           varchar     NOT NULL, \
        \    service_type   varchar     NOT NULL, \
        \    provider_uuid  uuid        NOT NULL, \
        \    external_id    varchar     NOT NULL, \
        \    external_label varchar, \
        \    email          varchar, \
        \    first_name     varchar, \
        \    last_name      varchar, \
        \    image_url      varchar, \
        \    affiliation    varchar, \
        \    tenant_uuid    uuid        NOT NULL, \
        \    created_at     timestamptz NOT NULL, \
        \    CONSTRAINT user_registration_pending_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT user_registration_pending_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE UNIQUE INDEX user_registration_pending_hash_uindex ON user_registration_pending (hash, tenant_uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addUserEmailVerification dbPool = do
  let sql =
        "ALTER TABLE user_entity ADD COLUMN email_verified_at timestamptz; \
        \ALTER TABLE user_entity ADD COLUMN email_pending varchar; \
        \UPDATE user_entity SET email_verified_at = created_at WHERE email_verified_at IS NULL;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropLegacyOpenIdConfig dbPool = do
  let sql =
        "ALTER TABLE user_entity DROP COLUMN sources; \
        \DROP TABLE config_authentication_openid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addNonAdminLoginEnabled dbPool = do
  let sql = "ALTER TABLE config_authentication ADD COLUMN IF NOT EXISTS internal_non_admin_login_enabled bool NOT NULL DEFAULT true;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addConfigMailCustomTemplates dbPool = do
  let sql = "ALTER TABLE config_mail ADD COLUMN custom_templates bool NOT NULL DEFAULT false;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addAuthExpirationColumns dbPool = do
  let sql =
        "ALTER TABLE config_authentication ADD COLUMN IF NOT EXISTS internal_session_expiration bigint NOT NULL DEFAULT 336; \
        \ALTER TABLE config_authentication ADD COLUMN IF NOT EXISTS internal_user_email_link_expiration bigint NOT NULL DEFAULT 336;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameUserEmailLinkTable dbPool = do
  let sql =
        "ALTER TABLE IF EXISTS action_key RENAME TO user_email_link; \
        \ALTER TABLE IF EXISTS user_email_link RENAME CONSTRAINT action_key_pk TO user_email_link_pk; \
        \ALTER TABLE IF EXISTS user_email_link RENAME CONSTRAINT action_key_identity_fk TO user_email_link_identity_fk; \
        \ALTER TABLE IF EXISTS user_email_link RENAME CONSTRAINT action_key_tenant_uuid_fk TO user_email_link_tenant_uuid_fk; \
        \UPDATE user_email_link SET type = 'RegistrationUserEmailLinkType' WHERE type = 'RegistrationActionKey'; \
        \UPDATE user_email_link SET type = 'ForgottenPasswordUserEmailLinkType' WHERE type = 'ForgottenPasswordActionKey'; \
        \UPDATE user_email_link SET type = 'TwoFactorAuthUserEmailLinkType' WHERE type = 'TwoFactorAuthActionKey'; \
        \UPDATE user_email_link SET type = 'ConsentsRequiredUserEmailLinkType' WHERE type = 'ConsentsRequiredActionKey'; \
        \UPDATE user_email_link SET type = 'EmailChangeUserEmailLinkType' WHERE type = 'EmailChangeActionKey';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
