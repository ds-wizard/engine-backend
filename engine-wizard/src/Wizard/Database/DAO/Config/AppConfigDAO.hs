module Wizard.Database.DAO.Config.AppConfigDAO where

import Control.Lens ((^.))
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Config.AppConfig ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

entityName = "app_config"

findAppConfig :: AppContextM AppConfig
findAppConfig = createFindEntityByFn entityName "id" "1"

insertAppConfig :: AppConfig -> AppContextM Int64
insertAppConfig = createInsertFn entityName

updateAppConfig :: AppConfig -> AppContextM Int64
updateAppConfig config = do
  let params = toRow config ++ [toField $ config ^. aId]
  let sql =
        "UPDATE app_config SET id = ?, organization = ?, authentication = ?, privacy_and_support = ?, dashboard = ?, look_and_feel = ?, registry = ?, knowledge_model = ?, questionnaire = ?, template = ?, submission = ?, created_at = ?, updated_at = ? WHERE id = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteAppConfigs :: AppContextM Int64
deleteAppConfigs = createDeleteEntitiesFn entityName
