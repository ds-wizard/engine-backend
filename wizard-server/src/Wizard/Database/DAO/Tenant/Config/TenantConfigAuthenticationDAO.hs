module Wizard.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Sort
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
findTenantConfigAuthenticationByUuid tenantUuid = do
  config <- createFindEntityByFn "config_authentication" [("tenant_uuid", U.toString tenantUuid)]
  externalServices <- createFindEntitiesBySortedFn "config_authentication_openid" [("tenant_uuid", U.toString tenantUuid)] [Sort "id" Ascending]
  return $ config {external = config.external {services = externalServices}}

insertTenantConfigAuthentication :: TenantConfigAuthentication -> AppContextM Int64
insertTenantConfigAuthentication = createInsertFn "config_authentication"

insertTenantConfigAuthenticationExternalService :: TenantConfigAuthenticationExternalService -> AppContextM Int64
insertTenantConfigAuthenticationExternalService = createInsertFn "config_authentication_openid"

updateTenantConfigAuthentication :: TenantConfigAuthentication -> AppContextM Int64
updateTenantConfigAuthentication config = do
  let sql =
        fromString $
          "UPDATE config_authentication SET tenant_uuid = ?, default_role = ?, internal_registration_enabled = ?, internal_two_factor_auth_enabled = ?, internal_two_factor_auth_code_length = ?, internal_two_factor_auth_code_expiration = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?; \
          \DELETE FROM config_authentication_openid WHERE tenant_uuid = ?;"
            ++ concatMap (const "INSERT INTO config_authentication_openid VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);") config.external.services
  let params =
        toRow config
          ++ [toField config.tenantUuid, toField config.tenantUuid]
          ++ concatMap toRow config.external.services
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigAuthentications :: AppContextM Int64
deleteTenantConfigAuthentications = do
  createDeleteEntitiesFn "config_authentication_openid"
  createDeleteEntitiesFn "config_authentication"
