module WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigLookAndFeel ()
import WizardLib.Public.Model.Tenant.Config.TenantConfig

findTenantConfigLookAndFeel :: AppContextC s sc m => m TenantConfigLookAndFeel
findTenantConfigLookAndFeel = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigLookAndFeelByUuid tenantUuid

findTenantConfigLookAndFeelByUuid :: AppContextC s sc m => U.UUID -> m TenantConfigLookAndFeel
findTenantConfigLookAndFeelByUuid tenantUuid = do
  config <- createFindEntityByFn "config_look_and_feel" [("tenant_uuid", U.toString tenantUuid)]
  customMenuLinks <- createFindEntitiesBySortedFn "config_look_and_feel_custom_menu_link" [("tenant_uuid", U.toString tenantUuid)] [Sort "position" Ascending]
  return $ config {customMenuLinks = customMenuLinks}

insertTenantConfigLookAndFeel :: AppContextC s sc m => TenantConfigLookAndFeel -> m Int64
insertTenantConfigLookAndFeel = createInsertFn "config_look_and_feel"

insertTenantConfigLookAndFeelCustomMenuLink :: AppContextC s sc m => TenantConfigLookAndFeelCustomMenuLink -> m Int64
insertTenantConfigLookAndFeelCustomMenuLink = createInsertFn "config_look_and_feel_custom_menu_link"

updateTenantConfigLookAndFeel :: AppContextC s sc m => TenantConfigLookAndFeel -> m Int64
updateTenantConfigLookAndFeel config = do
  let sql =
        fromString $
          "UPDATE config_look_and_feel SET tenant_uuid = ?, app_title = ?, app_title_short = ?, logo_url = ?, primary_color = ?, illustration_color = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?; \
          \DELETE FROM config_look_and_feel_custom_menu_link WHERE tenant_uuid = ?;"
            ++ concatMap (const "INSERT INTO config_look_and_feel_custom_menu_link VALUES (?, ?, ?, ?, ?, ?, ?, ?);") config.customMenuLinks
  let params =
        toRow config
          ++ [toField config.tenantUuid, toField config.tenantUuid]
          ++ concatMap toRow config.customMenuLinks
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigLookAndFeels :: AppContextC s sc m => m Int64
deleteTenantConfigLookAndFeels = do
  createDeleteEntitiesFn "config_look_and_feel_custom_menu_link"
  createDeleteEntitiesFn "config_look_and_feel"
