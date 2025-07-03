module Wizard.Database.DAO.Tenant.Config.TenantConfigRegistryDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigRegistry ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig

entityName = "config_registry"

findTenantConfigRegistry :: AppContextM TenantConfigRegistry
findTenantConfigRegistry = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigRegistryByUuid tenantUuid

findTenantConfigRegistryByUuid :: U.UUID -> AppContextM TenantConfigRegistry
findTenantConfigRegistryByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigRegistry :: TenantConfigRegistry -> AppContextM Int64
insertTenantConfigRegistry = createInsertFn entityName

updateTenantConfigRegistry :: TenantConfigRegistry -> AppContextM Int64
updateTenantConfigRegistry config = do
  let sql = fromString "UPDATE config_registry SET tenant_uuid = ?, enabled = ?, token = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigRegistries :: AppContextM Int64
deleteTenantConfigRegistries = createDeleteEntitiesFn entityName
