module WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigFeatures ()
import WizardLib.Public.Model.Tenant.Config.TenantConfig

entityName = "config_features"

findTenantConfigFeatures :: AppContextC s sc m => m TenantConfigFeatures
findTenantConfigFeatures = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigFeaturesByUuid tenantUuid

findTenantConfigFeaturesByUuid :: AppContextC s sc m => U.UUID -> m TenantConfigFeatures
findTenantConfigFeaturesByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigFeatures :: AppContextC s sc m => TenantConfigFeatures -> m Int64
insertTenantConfigFeatures = createInsertFn entityName

updateTenantConfigFeatures :: AppContextC s sc m => TenantConfigFeatures -> m Int64
updateTenantConfigFeatures config = do
  let sql =
        fromString
          "UPDATE config_features SET tenant_uuid = ?, ai_assistant_enabled = ?, tours_enabled = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigFeatures :: AppContextC s sc m => m Int64
deleteTenantConfigFeatures = createDeleteEntitiesFn entityName
