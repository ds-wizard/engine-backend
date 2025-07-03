module Wizard.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigDashboardAndLoginScreen ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.Tenant.Config.TenantConfig

findTenantConfigDashboardAndLoginScreen :: AppContextM TenantConfigDashboardAndLoginScreen
findTenantConfigDashboardAndLoginScreen = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigDashboardAndLoginScreenByUuid tenantUuid

findTenantConfigDashboardAndLoginScreenByUuid :: U.UUID -> AppContextM TenantConfigDashboardAndLoginScreen
findTenantConfigDashboardAndLoginScreenByUuid tenantUuid = do
  config <- createFindEntityByFn "config_dashboard_and_login_screen" [("tenant_uuid", U.toString tenantUuid)]
  announcements <- createFindEntitiesBySortedFn "config_dashboard_and_login_screen_announcement" [("tenant_uuid", U.toString tenantUuid)] [Sort "position" Ascending]
  return $ config {announcements = announcements}

insertTenantConfigDashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen -> AppContextM Int64
insertTenantConfigDashboardAndLoginScreen = createInsertFn "config_dashboard_and_login_screen"

insertTenantConfigDashboardAndLoginScreenAnnouncement :: TenantConfigDashboardAndLoginScreenAnnouncement -> AppContextM Int64
insertTenantConfigDashboardAndLoginScreenAnnouncement = createInsertFn "config_dashboard_and_login_screen_announcement"

updateTenantConfigDashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen -> AppContextM Int64
updateTenantConfigDashboardAndLoginScreen config = do
  let sql =
        fromString $
          "UPDATE config_dashboard_and_login_screen SET tenant_uuid = ?, dashboard_type = ?, login_info = ?, login_info_sidebar = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?; \
          \DELETE FROM config_dashboard_and_login_screen_announcement WHERE tenant_uuid = ?;"
            ++ concatMap (const "INSERT INTO config_dashboard_and_login_screen_announcement VALUES (?, ?, ?, ?, ?, ?, ?, ?);") config.announcements
  let params =
        toRow config
          ++ [toField config.tenantUuid, toField config.tenantUuid]
          ++ concatMap toRow config.announcements
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigDashboardAndLoginScreens :: AppContextM Int64
deleteTenantConfigDashboardAndLoginScreens = do
  createDeleteEntitiesFn "config_dashboard_and_login_screen_announcement"
  createDeleteEntitiesFn "config_dashboard_and_login_screen"
