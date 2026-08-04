module Wizard.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigAuthentication ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig

findTenantConfigAuthentication :: AppContextM TenantConfigAuthentication
findTenantConfigAuthentication = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigAuthenticationByUuid tenantUuid

findTenantConfigAuthenticationByUuid :: U.UUID -> AppContextM TenantConfigAuthentication
findTenantConfigAuthenticationByUuid tenantUuid =
  createFindEntityByFn "config_authentication" [("tenant_uuid", U.toString tenantUuid)]

insertTenantConfigAuthentication :: TenantConfigAuthentication -> AppContextM Int64
insertTenantConfigAuthentication = createInsertFn "config_authentication"

updateTenantConfigAuthentication :: TenantConfigAuthentication -> AppContextM Int64
updateTenantConfigAuthentication config = do
  let sql =
        fromString
          "UPDATE config_authentication SET tenant_uuid = ?, default_role_uuid = ?, internal_registration_enabled = ?, internal_two_factor_auth_enabled = ?, internal_two_factor_auth_code_length = ?, internal_two_factor_auth_code_expiration = ?, created_at = ?, updated_at = ?, internal_non_admin_login_enabled = ?, internal_session_expiration = ?, internal_user_email_link_expiration = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigAuthentications :: AppContextM Int64
deleteTenantConfigAuthentications = createDeleteEntitiesFn "config_authentication"
