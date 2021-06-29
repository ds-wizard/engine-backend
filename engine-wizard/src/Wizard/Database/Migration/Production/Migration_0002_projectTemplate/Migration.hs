module Wizard.Database.Migration.Production.Migration_0002_projectTemplate.Migration
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
    {mmNumber = 2, mmName = "Project Template", mmDescription = "Allow to create reusable templates for projects"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  updateAppConfig dbPool
  addFieldsToQuestionnaire dbPool
  addPermission dbPool

updateAppConfig dbPool = do
  let sql =
        "UPDATE app_config \
             \SET questionnaire=jsonb_set(to_jsonb(questionnaire), '{questionnaireCreation}', '\"TemplateAndCustomQuestionnaireCreation\"', true) \
             \WHERE id = 1"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addFieldsToQuestionnaire dbPool = do
  let sql =
        "ALTER TABLE questionnaire \
             \ADD description text, \
             \ADD is_template bool not null default false"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addPermission dbPool = do
  let sql =
        "UPDATE user_entity set permissions = permissions || '{QTN_TML_PERM}' WHERE role = 'admin' OR role = 'dataSteward'"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
