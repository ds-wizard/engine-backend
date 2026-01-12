module Wizard.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO where

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
import Wizard.Database.Mapping.Tenant.PluginSettings.TenantPluginSettings ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.PluginSettings.TenantPluginSettings

entityName = "tenant_plugin_settings"

findTenantPluginSettingValues :: U.UUID -> AppContextM (M.Map U.UUID A.Value)
findTenantPluginSettingValues tenantUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT plugin_uuid, values \
          \FROM tenant_plugin_settings \
          \WHERE tenant_uuid = ?"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  results <- runDB action
  return . M.fromList $ results

findTenantPluginSettingsByPluginUuid :: U.UUID -> AppContextM TenantPluginSettings
findTenantPluginSettingsByPluginUuid pluginUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("plugin_uuid", U.toString pluginUuid)]

findTenantPluginSettingsByPluginUuid' :: U.UUID -> AppContextM (Maybe TenantPluginSettings)
findTenantPluginSettingsByPluginUuid' pluginUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("plugin_uuid", U.toString pluginUuid)]

insertTenantPluginSettings :: TenantPluginSettings -> AppContextM Int64
insertTenantPluginSettings = createInsertFn entityName

updateTenantPluginSettings :: TenantPluginSettings -> AppContextM Int64
updateTenantPluginSettings pluginSettings = do
  let sql =
        fromString
          "UPDATE tenant_plugin_settings SET tenant_uuid = ?,  plugin_uuid = ?,  values = ?,  created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND plugin_uuid = ?"
  let params = toRow pluginSettings ++ [toField pluginSettings.tenantUuid, toField pluginSettings.pluginUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
