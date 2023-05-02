module Registry.Database.Migration.Production.Migration_0001_init.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 1, mmName = "Init database", mmDescription = "Create tables and load basic data"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createActionKeyTable dbPool
  createAuditTable dbPool
  createOrganizationTable dbPool
  createPackageTable dbPool
  createTemplateTable dbPool
  createTemplateFileTable dbPool
  createTemplateAssetTable dbPool
  insertOrganization dbPool

createActionKeyTable dbPool = do
  let sql =
        "create table action_key \
        \     ( \
        \         uuid            uuid not null \
        \             constraint action_key_pk \
        \                 primary key, \
        \         organization_id varchar, \
        \         type            varchar, \
        \         hash            varchar, \
        \         created_at      timestamp with time zone \
        \     ); \
        \     create unique index action_key_uuid_uindex \
        \         on action_key (uuid); \
        \     create unique index action_key_hash_uindex \
        \         on action_key (hash); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createAuditTable dbPool = do
  let sql =
        "create table audit \
        \ ( \
        \     type                varchar                  not null, \
        \     organization_id     varchar                  not null, \
        \     instance_statistics json                     not null, \
        \     package_id          varchar                  not null, \
        \     created_at          timestamp with time zone not null \
        \ );"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createOrganizationTable dbPool = do
  let sql =
        "create table organization \
        \ ( \
        \     organization_id varchar                  not null \
        \         constraint organization_pk \
        \             primary key, \
        \     name            varchar                  not null, \
        \     description     varchar                  not null, \
        \     email           varchar                  not null, \
        \     role            varchar                  not null, \
        \     token           varchar                  not null, \
        \     active          boolean                  not null, \
        \     logo            varchar, \
        \     created_at      timestamp with time zone not null, \
        \     updated_at      timestamp with time zone not null \
        \ ); \
        \create unique index organization_organization_id_uindex \
        \    on organization (organization_id); \
        \create unique index organization_token_uindex \
        \    on organization (token); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createPackageTable dbPool = do
  let sql =
        "create table package \
        \ ( \
        \     id                          varchar                  not null \
        \         constraint package_pk \
        \             primary key, \
        \     name                        varchar                  not null, \
        \     organization_id             varchar                  not null, \
        \     km_id                       varchar                  not null, \
        \     version                     varchar                  not null, \
        \     metamodel_version           integer                  not null, \
        \     description                 varchar                  not null, \
        \     readme                      varchar                  not null, \
        \     license                     varchar                  not null, \
        \     previous_package_id         varchar, \
        \     fork_of_package_id          varchar, \
        \     merge_checkpoint_package_id varchar, \
        \     events                      json                     not null, \
        \     created_at                  timestamp with time zone not null \
        \ ); \
        \create unique index package_id_uindex \
        \     on package (id); \
        \create index package_organization_id_km_id_index \
        \     on package (organization_id, km_id); \
        \create index package_previous_package_id_index \
        \     on package (previous_package_id);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTemplateTable dbPool = do
  let sql =
        "create table template \
        \ ( \
        \     id                     varchar                  not null \
        \         constraint template_pk \
        \             primary key, \
        \     name                   varchar                  not null, \
        \     organization_id        varchar                  not null, \
        \     template_id            varchar                  not null, \
        \     version                varchar                  not null, \
        \     metamodel_version      integer                  not null, \
        \     description            varchar                  not null, \
        \     readme                 varchar                  not null, \
        \     license                varchar                  not null, \
        \     allowed_packages       json                     not null, \
        \     recommended_package_id varchar, \
        \     formats                json                     not null, \
        \     created_at             timestamp with time zone not null \
        \ ); \
        \create unique index template_id_uindex \
        \     on template (id); \
        \create index template_organization_id_template_id_index \
        \     on template (organization_id, template_id); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTemplateFileTable dbPool = do
  let sql =
        " create table template_file \
        \ ( \
        \   template_id varchar not null, \
        \   uuid uuid not null, \
        \   file_name varchar not null, \
        \   content varchar not null \
        \ ); \
        \  \
        \ alter table template_file \
        \   add constraint template_file_template_id_fk \
        \      foreign key (template_id) references template (id); \
        \  \
        \ create unique index template_file_uuid_uindex \
        \   on template_file (uuid); \
        \  \
        \ alter table template_file \
        \   add constraint template_file_pk \
        \      primary key (uuid); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTemplateAssetTable dbPool = do
  let sql =
        " create table template_asset \
        \ ( \
        \   template_id varchar not null, \
        \   uuid uuid not null, \
        \   file_name varchar not null, \
        \   content_type varchar not null \
        \ ); \
        \  \
        \ alter table template_asset \
        \   add constraint template_asset_template_id_fk \
        \      foreign key (template_id) references template (id); \
        \  \
        \ create unique index template_asset_uuid_uindex \
        \   on template_asset (uuid); \
        \  \
        \ alter table template_asset \
        \   add constraint template_asset_pk \
        \      primary key (uuid); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertOrganization dbPool = do
  let sql =
        "INSERT INTO public.organization (organization_id, name, description, email, role, token, active, logo, created_at, \
        \                                  updated_at) \
        \ VALUES ('organization', 'Organization name', 'Some description of Organization', 'organization@example.com', \
        \         'AdminRole', 'GlobalToken', true, \
        \         'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+P+/HgAFhAJ/wlseKgAAAABJRU5ErkJggg==', \
        \         '2021-03-15 00:00:00.000000', '2021-03-15 00:00:00.000000');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
