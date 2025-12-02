module Wizard.Database.Migration.Development.User.UserSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/User) drop tables"
  let sql =
        "DROP TABLE IF EXISTS user_tour CASCADE;\
        \DROP TABLE IF EXISTS user_group_membership CASCADE;\
        \DROP TABLE IF EXISTS user_group CASCADE;\
        \DROP TABLE IF EXISTS user_token CASCADE;\
        \DROP TABLE IF EXISTS user_entity_submission_prop CASCADE; \
        \DROP TABLE IF EXISTS user_entity CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createUserTable
  createUserSubmissionPropsTable
  createUserTokenTable
  createUserGroupTable
  createUserGroupMembershipTable
  createUserTourTable

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
        \    sources           varchar[]   NOT NULL, \
        \    role              varchar     NOT NULL, \
        \    permissions       text[]      NOT NULL, \
        \    active            boolean     NOT NULL, \
        \    image_url         varchar, \
        \    last_visited_at   timestamptz NOT NULL, \
        \    created_at        timestamptz NOT NULL, \
        \    updated_at        timestamptz NOT NULL, \
        \    tenant_uuid       uuid        NOT NULL, \
        \    machine           boolean     NOT NULL, \
        \    locale            varchar, \
        \    CONSTRAINT user_entity_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT user_entity_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE UNIQUE INDEX user_email_uindex ON user_entity (email, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action

createUserLocaleForeignKeyConstraint :: AppContextM Int64
createUserLocaleForeignKeyConstraint = do
  logInfo _CMP_MIGRATION "(Table/User) create tables"
  let sql =
        "ALTER TABLE user_entity ADD CONSTRAINT user_entity_locale_fk FOREIGN KEY (locale, tenant_uuid) REFERENCES locale(id, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action

createUserSubmissionPropsTable = do
  logInfo _CMP_MIGRATION "(Table/UserSubmissionProp) create tables"
  let sql =
        "CREATE TABLE user_entity_submission_prop \
        \( \
        \    user_uuid   uuid        NOT NULL, \
        \    service_id  varchar     NOT NULL, \
        \    values      jsonb       NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    CONSTRAINT user_entity_submission_prop_pk PRIMARY KEY (user_uuid, service_id), \
        \    CONSTRAINT user_entity_submission_prop_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_entity_submission_prop_service_id_fk FOREIGN KEY (tenant_uuid, service_id) REFERENCES config_submission_service (tenant_uuid, id) ON DELETE CASCADE, \
        \    CONSTRAINT user_entity_submission_prop_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
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
        \    CONSTRAINT user_token_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT user_token_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_token_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
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
        \    CONSTRAINT user_group_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT user_group_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
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
        \    CONSTRAINT user_group_membership_pk PRIMARY KEY (user_group_uuid, user_uuid), \
        \    CONSTRAINT user_group_membership_user_group_uuid_fk FOREIGN KEY (user_group_uuid) REFERENCES user_group (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_group_membership_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_group_membership_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createUserTourTable = do
  logInfo _CMP_MIGRATION "(Table/UserTour) create tables"
  let sql =
        "CREATE TABLE user_tour \
        \( \
        \    user_uuid               uuid        NOT NULL, \
        \    tour_id                 varchar     NOT NULL, \
        \    tenant_uuid             uuid        NOT NULL, \
        \    created_at              timestamptz NOT NULL, \
        \    CONSTRAINT user_tour_pk PRIMARY KEY (user_uuid, tour_id), \
        \    CONSTRAINT user_tour_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_tour_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
