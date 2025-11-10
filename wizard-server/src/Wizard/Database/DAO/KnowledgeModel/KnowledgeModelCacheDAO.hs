module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO where

import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Cache.KnowledgeModelCache ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.KnowledgeModelCache

entityName = "knowledge_model_cache"

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
