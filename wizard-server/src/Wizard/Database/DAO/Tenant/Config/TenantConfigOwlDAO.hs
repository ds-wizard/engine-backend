module Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigOwl ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig

entityName = "config_owl"

findTenantConfigOwl :: AppContextM TenantConfigOwl
findTenantConfigOwl = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigOwlByUuid tenantUuid

findTenantConfigOwlByUuid :: U.UUID -> AppContextM TenantConfigOwl
findTenantConfigOwlByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigOwl :: TenantConfigOwl -> AppContextM Int64
insertTenantConfigOwl = createInsertFn entityName

updateTenantConfigOwl :: TenantConfigOwl -> AppContextM Int64
updateTenantConfigOwl config = do
  let sql = fromString "UPDATE config_owl SET tenant_uuid = ?, enabled = ?, name = ?, organization_id = ?, km_id = ?, version = ?, previous_package_id = ?, root_element = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigOwls :: AppContextM Int64
deleteTenantConfigOwls = createDeleteEntitiesFn entityName
