module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.KnowledgeModelCache ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.KnowledgeModelCache

entityName = "knowledge_model_cache"

findKnowledgeModelCaches :: AppContextM [KnowledgeModelCache]
findKnowledgeModelCaches = createFindEntitiesFn entityName

findKnowledgeModelCacheById' :: String -> [U.UUID] -> U.UUID -> AppContextM (Maybe KnowledgeModelCache)
findKnowledgeModelCacheById' packageId tagUuids tenantUuid = do
  let sql =
        fromString
          "SELECT * \
          \FROM knowledge_model_cache \
          \WHERE package_id = ? AND tag_uuids = ? AND tenant_uuid = ?"
  let params = [toField packageId, toField tagUuids, toField tenantUuid]
  let queryParams = [("package_id", packageId), ("tag_uuids", show tagUuids), ("tenant_uuid", U.toString tenantUuid)]
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB' entityName action queryParams

insertKnowledgeModelCache :: KnowledgeModelCache -> AppContextM Int64
insertKnowledgeModelCache = createInsertFn entityName

deleteKnowledgeModelCaches :: AppContextM Int64
deleteKnowledgeModelCaches = createDeleteEntitiesFn entityName

deleteKnowledgeModelCachesByPackageId :: String -> AppContextM Int64
deleteKnowledgeModelCachesByPackageId packageId = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [("package_id", packageId), ("tenant_uuid", U.toString tenantUuid)]

deleteKnowledgeModelCachesByPackageIds :: [String] -> AppContextM Int64
deleteKnowledgeModelCachesByPackageIds packageIds = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f'
            "DELETE \
            \FROM knowledge_model_cache \
            \WHERE package_id IN (%s) AND tenant_uuid = ?"
            [generateQuestionMarks packageIds]
  let params = packageIds ++ [U.toString tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
