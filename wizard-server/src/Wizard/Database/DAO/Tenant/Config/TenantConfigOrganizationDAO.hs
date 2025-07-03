module Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Wizard.Database.Mapping.Tenant.Config.TenantConfigOrganization ()
import Wizard.Model.Tenant.Config.TenantConfig

entityName = "config_organization"

findTenantConfigOrganization :: AppContextC s sc m => m TenantConfigOrganization
findTenantConfigOrganization = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigOrganizationByUuid tenantUuid

findTenantConfigOrganizationByUuid :: AppContextC s sc m => U.UUID -> m TenantConfigOrganization
findTenantConfigOrganizationByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigOrganization :: AppContextC s sc m => TenantConfigOrganization -> m Int64
insertTenantConfigOrganization = createInsertFn entityName

updateTenantConfigOrganization :: AppContextC s sc m => TenantConfigOrganization -> m Int64
updateTenantConfigOrganization config = do
  let sql =
        fromString
          "UPDATE config_organization SET tenant_uuid = ?, name = ?, description = ?, organization_id = ?, affiliations = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigOrganizations :: AppContextC s sc m => m Int64
deleteTenantConfigOrganizations = createDeleteEntitiesFn entityName
