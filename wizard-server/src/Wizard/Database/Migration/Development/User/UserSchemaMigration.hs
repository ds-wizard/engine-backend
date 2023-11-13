module Wizard.Database.Migration.Development.User.UserSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/User) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/User) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/User) drop tables"
  let sql =
        "DROP TABLE IF EXISTS user_group_membership CASCADE;\
        \DROP TABLE IF EXISTS user_group CASCADE;\
        \DROP TABLE IF EXISTS user_token CASCADE;\
        \DROP TABLE IF EXISTS user_entity CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createUserTable
  createUserTokenTable
  createUserGroupTable
  createUserGroupMembershipTable

createUserTable = do
  logInfo _CMP_MIGRATION "(Table/User) create tables"
  let sql =
        "CREATE TABLE user_entity \
        \( \
        \    uuid              uuid        NOT NULL, \
        \    first_name        varchar     NOT NULL, \
        \    last_name         varchar     NOT NULL, \
        \    email             varchar     NOT NULL, \
        \    password_hash     varchar     NOT NULL, \
        \    affiliation       varchar, \
        \    sources           json        NOT NULL, \
        \    role              varchar     NOT NULL, \
        \    permissions       text[]      NOT NULL, \
        \    active            boolean     NOT NULL, \
        \    submissions_props json        NOT NULL, \
        \    image_url         varchar, \
        \    last_visited_at   timestamptz NOT NULL, \
        \    created_at        timestamptz NOT NULL, \
        \    updated_at        timestamptz NOT NULL, \
        \    tenant_uuid       uuid        NOT NULL, \
        \    machine           boolean     NOT NULL, \
        \    CONSTRAINT user_entity_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT user_entity_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \); \
        \ \
        \CREATE UNIQUE INDEX user_email_uindex ON user_entity (email, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action

createUserTokenTable = do
  logInfo _CMP_MIGRATION "(Table/UserToken) create tables"
  let sql =
        "CREATE TABLE user_token \
        \( \
        \    uuid          uuid        NOT NULL, \
        \    user_uuid     uuid        NOT NULL, \
        \    value         varchar     NOT NULL, \
        \    session_state varchar, \
        \    tenant_uuid   uuid        NOT NULL, \
        \    created_at    timestamptz NOT NULL, \
        \    name          varchar     NOT NULL, \
        \    type          varchar     NOT NULL, \
        \    user_agent    varchar     NOT NULL, \
        \    expires_at    timestamptz NOT NULL, \
        \    CONSTRAINT user_token_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT user_token_user_uuid_fk FOREIGN KEY (user_uuid, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT user_token_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createUserGroupTable = do
  logInfo _CMP_MIGRATION "(Table/UserGroup) create tables"
  let sql =
        "CREATE TABLE user_group \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    name        varchar     NOT NULL, \
        \    description varchar, \
        \    private     boolean     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  TIMESTAMPTZ NOT NULL, \
        \    updated_at  TIMESTAMPTZ NOT NULL, \
        \    CONSTRAINT user_group_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT user_group_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createUserGroupMembershipTable = do
  logInfo _CMP_MIGRATION "(Table/UserGroupMembership) create tables"
  let sql =
        "CREATE TABLE user_group_membership \
        \( \
        \    user_group_uuid uuid        NOT NULL, \
        \    user_uuid       uuid        NOT NULL, \
        \    type            varchar     NOT NULL, \
        \    tenant_uuid     uuid        NOT NULL, \
        \    created_at      TIMESTAMPTZ NOT NULL, \
        \    updated_at      TIMESTAMPTZ NOT NULL, \
        \    CONSTRAINT user_group_membership_pk PRIMARY KEY (user_group_uuid, user_uuid, tenant_uuid), \
        \    CONSTRAINT user_group_membership_user_group_uuid_fk FOREIGN KEY (user_group_uuid, tenant_uuid) REFERENCES user_group (uuid, tenant_uuid), \
        \    CONSTRAINT user_group_membership_user_uuid_fk FOREIGN KEY (user_uuid, tenant_uuid) REFERENCES user_entity (uuid, tenant_uuid), \
        \    CONSTRAINT user_group_membership_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
