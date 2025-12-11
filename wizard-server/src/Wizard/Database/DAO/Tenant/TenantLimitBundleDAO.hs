module Wizard.Database.DAO.Tenant.TenantLimitBundleDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.TenantLimitBundle ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Limit.TenantLimitBundle

entityName = "tenant_limit_bundle"

findLimitBundleByUuid :: U.UUID -> AppContextM TenantLimitBundle
findLimitBundleByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

findLimitBundleForCurrentTenant :: AppContextM TenantLimitBundle
findLimitBundleForCurrentTenant = do
  tenantUuid <- asks currentTenantUuid
  findLimitBundleByUuid tenantUuid

insertLimitBundle :: TenantLimitBundle -> AppContextM Int64
insertLimitBundle = createInsertFn entityName

updateLimitBundleByUuid :: TenantLimitBundle -> AppContextM TenantLimitBundle
updateLimitBundleByUuid limitBundle = do
  now <- liftIO getCurrentTime
  let updatedTenantLimitBundle = limitBundle {updatedAt = now}
  let sql =
        fromString
          "UPDATE tenant_limit_bundle SET uuid = ?, users = ?, active_users = ?, knowledge_models = ?, knowledge_model_editors = ?, document_templates = ?, projects = ?, documents =?, storage = ?, created_at = ?, updated_at = ?, document_template_drafts = ?, locales = ? WHERE uuid = ?"
  let params = toRow updatedTenantLimitBundle ++ [toField updatedTenantLimitBundle.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedTenantLimitBundle

deleteLimitBundles :: AppContextM Int64
deleteLimitBundles = createDeleteEntitiesFn entityName
