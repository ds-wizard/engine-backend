module Registry.Database.Migration.Development.Template.TemplateSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.Template.TemplateS3
import Registry.Util.Logger
import Shared.Database.DAO.CommonSql

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Template) started"
  dropTables
  createTables
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())
  makeBucket
  logInfo _CMP_MIGRATION "(Table/Template) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Template) drop tables"
  let sql = "drop table if exists template;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Template) create table"
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
          \     files                  json                     not null, \
          \     assets                 json                     not null, \
          \     created_at             timestamp with time zone not null \
          \ ); \
          \create unique index template_id_uindex \
          \     on template (id); \
          \create index template_organization_id_template_id_index \
          \     on template (organization_id, template_id); "
  let action conn = execute_ conn sql
  runDB action
