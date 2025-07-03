module WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigMailDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigMail ()
import WizardLib.Public.Model.Tenant.Config.TenantConfig

entityName = "config_mail"

findTenantConfigMail :: AppContextC s sc m => m TenantConfigMail
findTenantConfigMail = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigMailByUuid tenantUuid

findTenantConfigMailByUuid :: AppContextC s sc m => U.UUID -> m TenantConfigMail
findTenantConfigMailByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigMail :: AppContextC s sc m => TenantConfigMail -> m Int64
insertTenantConfigMail = createInsertFn entityName

deleteTenantConfigMails :: AppContextC s sc m => m Int64
deleteTenantConfigMails = createDeleteEntitiesFn entityName
