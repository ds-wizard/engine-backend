module Wizard.Database.Migration.Production.Migration_0021_qtnImporter.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 21
    , mmName = "Add Questionnaire Importer"
    , mmDescription = "Add qtn importer table and QTN_IMPORTER_PERM to user"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createQtnImporterTable dbPool
  addQtnImporterPermission dbPool
  return Nothing

createQtnImporterTable dbPool = do
  let sql =
        "create table questionnaire_importer \
          \ ( \
          \     id                     varchar                  not null, \
          \     name                   varchar                  not null, \
          \     organization_id        varchar                  not null, \
          \     importer_id            varchar                  not null, \
          \     version                varchar                  not null, \
          \     metamodel_version      integer                  not null, \
          \     description            varchar                  not null, \
          \     readme                 varchar                  not null, \
          \     license                varchar                  not null, \
          \     allowed_packages       json                     not null, \
          \     url                    varchar, \
          \     enabled                bool                     not null, \
          \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
          \       constraint questionnaire_importer_app_uuid_fk \
          \         references app, \
          \     created_at             timestamp with time zone not null, \
          \     updated_at             timestamp with time zone not null \
          \ ); \
          \ \
          \alter table questionnaire_importer \
          \    add constraint questionnaire_importer_pk primary key (id, app_uuid); \
          \create unique index questionnaire_importer_id_uindex \
          \     on questionnaire_importer (id, app_uuid); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addQtnImporterPermission dbPool = do
  let sql =
        "UPDATE user_entity set permissions = permissions || '{QTN_IMPORTER_PERM}' WHERE role = 'admin' OR role = 'dataSteward'"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
