module Wizard.Database.Migration.Production.Migration_0010_app.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 10, mmName = "Add app", mmDescription = "Allow to have more app in one instance"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addAppTable dbPool
  addAppDefaultRecord dbPool
  changeToAppConfigUuid dbPool
  addForeignKey dbPool "action_key"
  addForeignKey dbPool "branch"
  addForeignKey dbPool "document"
  addForeignKey dbPool "document_queue"
  addForeignKey dbPool "feedback"
  addForeignKey dbPool "knowledge_model_migration"
  addForeignKey dbPool "package"
  addForeignKey dbPool "questionnaire"
  addForeignKey dbPool "questionnaire_migration"
  addForeignKey dbPool "submission"
  addForeignKey dbPool "template"
  addForeignKey dbPool "template_asset"
  addForeignKey dbPool "template_file"
  addForeignKey dbPool "user_entity"

addAppTable dbPool = do
  let sql =
        "CREATE TABLE app \
         \ ( \
         \     uuid              uuid              not null \
         \         constraint app_pk \
         \             primary key, \
         \     app_id            varchar           not null,\
         \     name              varchar           not null,\
         \     server_domain     varchar           not null,\
         \     client_domain     varchar           not null,\
         \     enabled           bool              not null,\
         \     created_at timestamp with time zone not null,\
         \     updated_at timestamp with time zone not null \
         \ ); \
         \  \
         \ CREATE UNIQUE INDEX app_uuid_uindex \
         \     ON app (uuid);"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addAppDefaultRecord dbPool = do
  let sql =
        "INSERT INTO app VALUES ('00000000-0000-0000-0000-000000000000', 'default', 'Default App', 'server.example.com', 'client.example.com', true, '2021-10-18 08:25:17.016000', '2021-10-18 08:25:18.326000');"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

changeToAppConfigUuid dbPool = do
  let sql =
        "ALTER TABLE app_config ALTER COLUMN id TYPE varchar USING id::varchar;\
         \update app_config SET id = '00000000-0000-0000-0000-000000000000' WHERE id = '1';\
         \ALTER TABLE app_config ALTER COLUMN id DROP default;\
         \ALTER TABLE app_config ALTER COLUMN id TYPE uuid USING id::uuid;\
         \ALTER TABLE app_config ALTER COLUMN id SET default '00000000-0000-0000-0000-000000000000';\
         \ALTER TABLE app_config rename column id to uuid;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addForeignKey dbPool table = do
  let sql =
        f'
          "ALTER TABLE %s \
           \  ADD app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null; \
           \ALTER TABLE %s \
           \  ADD CONSTRAINT %s_app_uuid_fk \
           \    FOREIGN KEY (app_uuid) REFERENCES app (uuid); "
          [table, table, table]
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------------------------------------
f' :: String -> [String] -> String
f' str terms =
  case str of
    '%':'s':rest -> (fromMaybe "%s" . listToMaybe $ terms) ++ f' rest (drop 1 terms)
    '%':'%':'s':rest -> '%' : 's' : f' rest terms
    a:rest -> a : f' rest terms
    [] -> []
