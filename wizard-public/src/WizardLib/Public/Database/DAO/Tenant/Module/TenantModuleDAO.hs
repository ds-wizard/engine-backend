module WizardLib.Public.Database.DAO.Tenant.Module.TenantModuleDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.Tenant.Module.TenantModule ()
import WizardLib.Public.Model.Tenant.Module.TenantModule

entityName = "tenant_module"

findTenantModules :: AppContextC s sc m => m [TenantModule]
findTenantModules = do
  tenantUuid <- asks (.tenantUuid')
  findTenantModulesByTenantUuid tenantUuid

findTenantModulesByTenantUuid :: AppContextC s sc m => U.UUID -> m [TenantModule]
findTenantModulesByTenantUuid tenantUuid =
  createFindEntitiesBySortedFn entityName [("tenant_uuid", U.toString tenantUuid)] [Sort "position" Ascending]

insertTenantModule :: AppContextC s sc m => TenantModule -> m Int64
insertTenantModule = createInsertFn entityName

deleteTenantModules :: AppContextC s sc m => m Int64
deleteTenantModules = createDeleteEntitiesFn entityName

deleteTenantModulesByTenantUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteTenantModulesByTenantUuid tenantUuid = createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid]
