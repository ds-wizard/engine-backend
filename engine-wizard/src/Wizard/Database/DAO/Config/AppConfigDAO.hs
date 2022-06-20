module Wizard.Database.DAO.Config.AppConfigDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
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

entityName = "app_config"

findAppConfig :: AppContextM AppConfig
findAppConfig = do
  appUuid <- asks _appContextAppUuid
  findAppConfigByUuid appUuid

findAppConfigByUuid :: U.UUID -> AppContextM AppConfig
findAppConfigByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

findAppConfigByClientUrl :: String -> AppContextM AppConfig
findAppConfigByClientUrl clientUrl = createFindEntityByFn entityName [("client_url", clientUrl)]

insertAppConfig :: AppConfig -> AppContextM Int64
insertAppConfig = createInsertFn entityName

updateAppConfig :: AppConfig -> AppContextM Int64
updateAppConfig config = do
  let sql =
        fromString
          "UPDATE app_config SET uuid = ?, organization = ?, authentication = ?, privacy_and_support = ?, dashboard = ?, look_and_feel = ?, registry = ?, knowledge_model = ?, questionnaire = ?, template = ?, submission = ?, created_at = ?, updated_at = ?, feature = ?, owl = ? WHERE uuid = ?"
  let params = toRow config ++ [toField $ config ^. uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteAppConfigs :: AppContextM Int64
deleteAppConfigs = createDeleteEntitiesFn entityName
