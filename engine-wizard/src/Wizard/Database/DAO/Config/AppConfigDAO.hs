module Wizard.Database.DAO.Config.AppConfigDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Config.AppConfig ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

entityName = "app_config"

findAppConfig :: AppContextM AppConfig
findAppConfig = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [("uuid", U.toString appUuid)]

insertAppConfig :: AppConfig -> AppContextM Int64
insertAppConfig = createInsertFn entityName

updateAppConfig :: AppConfig -> AppContextM Int64
updateAppConfig config = do
  appUuid <- asks _appContextAppUuid
  let params = toRow config ++ [toField appUuid]
  let sql =
        "UPDATE app_config SET uuid = ?, organization = ?, authentication = ?, privacy_and_support = ?, dashboard = ?, look_and_feel = ?, registry = ?, knowledge_model = ?, questionnaire = ?, template = ?, submission = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteAppConfigs :: AppContextM Int64
deleteAppConfigs = createDeleteEntitiesFn entityName
