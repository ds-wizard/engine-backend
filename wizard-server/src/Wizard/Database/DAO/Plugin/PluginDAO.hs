module Wizard.Database.DAO.Plugin.PluginDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Plugin.Plugin ()
import Wizard.Database.Mapping.Plugin.PluginList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Plugin.Plugin
import Wizard.Model.Plugin.PluginList

entityName = "plugin"

findPlugins :: U.UUID -> AppContextM [PluginList]
findPlugins tenantUuid = do
  createFindEntitiesWithFieldsByFn "uuid, url, enabled" entityName [tenantQueryUuid tenantUuid]

insertPlugin :: Plugin -> AppContextM Int64
insertPlugin = createInsertFn entityName

updatePluginEnabled :: U.UUID -> Bool -> AppContextM Int64
updatePluginEnabled uuid enabled = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE plugin SET enabled = ?, updated_at = now() WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField enabled, toField tenantUuid, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updatePluginUrlForAllTenants :: U.UUID -> String -> AppContextM Int64
updatePluginUrlForAllTenants uuid url = do
  let sql = fromString "UPDATE plugin SET url = ?, updated_at = now() WHERE uuid = ?"
  let params = [toField url, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updatePluginUrlForTenant :: U.UUID -> U.UUID -> String -> AppContextM Int64
updatePluginUrlForTenant tenantUuid uuid url = do
  let sql = fromString "UPDATE plugin SET url = ?, updated_at = now() WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField url, toField tenantUuid, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePlugins :: AppContextM Int64
deletePlugins = createDeleteEntitiesFn entityName

deletePluginForAllTenants :: U.UUID -> AppContextM Int64
deletePluginForAllTenants pluginUuid = createDeleteEntityByFn entityName [("uuid", U.toString pluginUuid)]

deletePluginForTenant :: U.UUID -> U.UUID -> AppContextM Int64
deletePluginForTenant tenantUuid pluginUuid = createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString pluginUuid)]
