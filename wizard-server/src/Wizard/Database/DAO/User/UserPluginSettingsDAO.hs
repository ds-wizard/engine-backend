module Wizard.Database.DAO.User.UserPluginSettingsDAO where

import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.User.UserPluginSettings ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.UserPluginSettings

entityName = "user_plugin_settings"

findUserPluginSettingValuesByUserUuidAndTenantUuid :: U.UUID -> U.UUID -> AppContextM (M.Map U.UUID A.Value)
findUserPluginSettingValuesByUserUuidAndTenantUuid userUuid tenantUuid = do
  let sql =
        fromString
          "SELECT plugin_uuid, values \
          \FROM user_plugin_settings \
          \WHERE tenant_uuid = ? \
          \  AND user_uuid = ?;"
  let params = [toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = query conn sql params
  results <- runDB action
  return . M.fromList $ results

findUserPluginSettingsByUserUuidAndPluginUuid :: U.UUID -> U.UUID -> AppContextM UserPluginSettings
findUserPluginSettingsByUserUuidAndPluginUuid userUuid pluginUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid), ("plugin_uuid", U.toString pluginUuid)]

findUserPluginSettingsByUserUuidAndPluginUuid' :: U.UUID -> U.UUID -> AppContextM (Maybe UserPluginSettings)
findUserPluginSettingsByUserUuidAndPluginUuid' userUuid pluginUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid), ("plugin_uuid", U.toString pluginUuid)]

insertUserPluginSettings :: UserPluginSettings -> AppContextM Int64
insertUserPluginSettings = createInsertFn entityName

updateUserPluginSettings :: UserPluginSettings -> AppContextM Int64
updateUserPluginSettings pluginSettings = do
  let sql =
        fromString
          "UPDATE user_plugin_settings SET user_uuid = ?, plugin_uuid = ?,  values = ?,  tenant_uuid = ?,  created_at = ?, updated_at = ? WHERE user_uuid = ? AND plugin_uuid = ?"
  let params = toRow pluginSettings ++ [toField pluginSettings.userUuid, toField pluginSettings.pluginUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
