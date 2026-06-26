module Wizard.Service.Tenant.Module.ModuleService where

import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import WizardLib.Public.Database.DAO.Tenant.Module.TenantModuleDAO
import WizardLib.Public.Model.Tenant.Module.TenantModule

createOrUpdateTenantModules :: U.UUID -> [TenantModule] -> AppContextM ()
createOrUpdateTenantModules tenantUuid modules =
  runInTransaction $ do
    deleteTenantModulesByTenantUuid tenantUuid
    traverse_ insertTenantModule modules
