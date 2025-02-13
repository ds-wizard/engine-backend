module Wizard.Database.DAO.Tenant.TenantDAO where

import Control.Monad.Reader (liftIO)
import qualified Data.List as L
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Tenant ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Tenant

entityName = "tenant"

pageLabel = "tenants"

findTenants :: AppContextM [Tenant]
findTenants = createFindEntitiesFn entityName

findTenantsPage :: Maybe String -> Maybe [TenantState] -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page Tenant)
findTenantsPage mQuery mStates mEnabled pageable sort = do
  let statesCondition =
        case mStates of
          Nothing -> ""
          Just [] -> ""
          Just states ->
            let xs = fmap (\s -> f' "state = '%s'" [show s]) states
             in " AND (" ++ L.intercalate " OR " xs ++ ")"
  let enabledCondition =
        case mEnabled of
          Nothing -> ""
          Just True -> " AND enabled = true"
          Just False -> " AND enabled = false"
  let condition = f' "WHERE (name ~* ? OR tenant_id ~* ?) %s %s" [statesCondition, enabledCondition]
  createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort "*" condition [regexM mQuery, regexM mQuery]

findTenantByUuid :: U.UUID -> AppContextM Tenant
findTenantByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

findTenantByServerDomain :: String -> AppContextM Tenant
findTenantByServerDomain serverDomain = createFindEntityByFn entityName [("server_domain", serverDomain)]

findTenantByClientUrl :: String -> AppContextM Tenant
findTenantByClientUrl clientUrl = createFindEntityByFn entityName [("client_url", clientUrl)]

insertTenant :: Tenant -> AppContextM Int64
insertTenant = createInsertFn entityName

updateTenantByUuid :: Tenant -> AppContextM Tenant
updateTenantByUuid tenant = do
  now <- liftIO getCurrentTime
  let updatedTenant = tenant {updatedAt = now}
  let sql =
        fromString
          "UPDATE tenant SET uuid = ?, tenant_id = ?, name = ?, server_domain = ?, client_url = ?, enabled = ?, created_at = ?, updated_at = ?, server_url = ?, admin_server_url = ?, admin_client_url = ?, integration_hub_server_url = ?, integration_hub_client_url = ?, analytics_server_url = ?, analytics_client_url = ?, signal_bridge_url = ?, state = ? WHERE uuid = ?"
  let params = toRow tenant ++ [toField updatedTenant.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedTenant

deleteTenants :: AppContextM Int64
deleteTenants = createDeleteEntitiesFn entityName

deleteTenantByUuid :: U.UUID -> AppContextM Int64
deleteTenantByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
