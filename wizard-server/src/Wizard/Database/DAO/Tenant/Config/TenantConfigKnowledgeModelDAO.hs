module Wizard.Database.DAO.Tenant.Config.TenantConfigKnowledgeModelDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigKnowledgeModel ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig

findTenantConfigKnowledgeModel :: AppContextM TenantConfigKnowledgeModel
findTenantConfigKnowledgeModel = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigKnowledgeModelByUuid tenantUuid

findTenantConfigKnowledgeModelByUuid :: U.UUID -> AppContextM TenantConfigKnowledgeModel
findTenantConfigKnowledgeModelByUuid tenantUuid = do
  config <- createFindEntityByFn "config_knowledge_model" [("tenant_uuid", U.toString tenantUuid)]
  knowledgeModelPackages <- createFindEntitiesBySortedFn "config_knowledge_model_public_package_pattern" [("tenant_uuid", U.toString tenantUuid)] [Sort "position" Ascending]
  return $ config {public = config.public {knowledgeModelPackages = knowledgeModelPackages}}

insertTenantConfigKnowledgeModel :: TenantConfigKnowledgeModel -> AppContextM Int64
insertTenantConfigKnowledgeModel = createInsertFn "config_knowledge_model"

insertTenantConfigKnowledgeModelPublicPackagePattern :: TenantConfigKnowledgeModelPublicPackagePattern -> AppContextM Int64
insertTenantConfigKnowledgeModelPublicPackagePattern = createInsertFn "config_knowledge_model_public_package_pattern"

updateTenantConfigKnowledgeModel :: TenantConfigKnowledgeModel -> AppContextM Int64
updateTenantConfigKnowledgeModel config = do
  let sql =
        fromString $
          "UPDATE config_knowledge_model SET tenant_uuid = ?, public_enabled = ?, integration_config = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?; \
          \DELETE FROM config_knowledge_model_public_package_pattern WHERE tenant_uuid = ?;"
            ++ concatMap (const "INSERT INTO config_knowledge_model_public_package_pattern VALUES (?, ?, ?, ?, ?, ?, ?, ?);") config.public.knowledgeModelPackages
  let params =
        toRow config
          ++ [toField config.tenantUuid, toField config.tenantUuid]
          ++ concatMap toRow config.public.knowledgeModelPackages
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigKnowledgeModels :: AppContextM Int64
deleteTenantConfigKnowledgeModels = do
  createDeleteEntitiesFn "config_knowledge_model_public_package_pattern"
  createDeleteEntitiesFn "config_knowledge_model"
