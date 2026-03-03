module Wizard.Database.DAO.Tenant.Config.TenantConfigKnowledgeModelDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

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
  createFindEntityByFn "config_knowledge_model" [("tenant_uuid", U.toString tenantUuid)]

insertTenantConfigKnowledgeModel :: TenantConfigKnowledgeModel -> AppContextM Int64
insertTenantConfigKnowledgeModel = createInsertFn "config_knowledge_model"

updateTenantConfigKnowledgeModel :: TenantConfigKnowledgeModel -> AppContextM Int64
updateTenantConfigKnowledgeModel config = do
  let sql = fromString "UPDATE config_knowledge_model SET tenant_uuid = ?, integration_config = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?;"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigKnowledgeModels :: AppContextM Int64
deleteTenantConfigKnowledgeModels = do
  createDeleteEntitiesFn "config_knowledge_model"
