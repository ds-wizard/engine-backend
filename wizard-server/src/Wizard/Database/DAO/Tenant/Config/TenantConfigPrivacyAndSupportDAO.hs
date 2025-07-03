module Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigPrivacyAndSupport ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig

entityName = "config_privacy_and_support"

findTenantConfigPrivacyAndSupport :: AppContextM TenantConfigPrivacyAndSupport
findTenantConfigPrivacyAndSupport = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigPrivacyAndSupportByUuid tenantUuid

findTenantConfigPrivacyAndSupportByUuid :: U.UUID -> AppContextM TenantConfigPrivacyAndSupport
findTenantConfigPrivacyAndSupportByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigPrivacyAndSupport :: TenantConfigPrivacyAndSupport -> AppContextM Int64
insertTenantConfigPrivacyAndSupport = createInsertFn entityName

updateTenantConfigPrivacyAndSupport :: TenantConfigPrivacyAndSupport -> AppContextM Int64
updateTenantConfigPrivacyAndSupport config = do
  let sql = fromString "UPDATE config_privacy_and_support SET tenant_uuid = ?, privacy_url = ?, terms_of_service_url = ?, support_email = ?, support_site_name = ?, support_site_url = ?, support_site_icon = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigPrivacyAndSupports :: AppContextM Int64
deleteTenantConfigPrivacyAndSupports = createDeleteEntitiesFn entityName
