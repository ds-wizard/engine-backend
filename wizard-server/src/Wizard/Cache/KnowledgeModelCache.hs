module Wizard.Cache.KnowledgeModelCache where

import Control.Monad.Reader (liftIO)
import Data.Hashable
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.String
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.KnowledgeModelCache
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.Public.Service.Cache.Common

cacheName = "KnowledgeModel"

cacheKey :: String -> [U.UUID] -> U.UUID -> String
cacheKey packageId tagUuids tenantUuid =
  f''
    "packageId: '${packageId}', tagUuids: '${tagUuids}', tenantUuid: '${tenantUuid}'"
    [ ("packageId", packageId)
    , ("tagUuids", show . hash $ tagUuids)
    , ("tenantUuid", U.toString tenantUuid)
    ]

addToCache :: (String, [U.UUID], U.UUID) -> KnowledgeModel -> AppContextM ()
addToCache (packageId, tagUuids, tenantUuid) knowledgeModel = do
  createdAt <- liftIO getCurrentTime
  let cacheRecord = KnowledgeModelCache {packageId = packageId, tagUuids = tagUuids, knowledgeModel = knowledgeModel, tenantUuid = tenantUuid, createdAt = createdAt}
  let key = cacheKey packageId tagUuids tenantUuid
  logCacheAddBefore cacheName key
  insertKnowledgeModelCache cacheRecord
  logCacheAddAfter cacheName key
  return ()

getFromCache :: (String, [U.UUID], U.UUID) -> AppContextM (Maybe KnowledgeModel)
getFromCache (packageId, tagUuids, tenantUuid) = do
  let key = cacheKey packageId tagUuids tenantUuid
  logCacheGetBefore cacheName key
  mValue <- findKnowledgeModelCacheById' packageId tagUuids tenantUuid
  case mValue of
    Just value -> do
      logCacheGetFound cacheName key
      return . Just $ value.knowledgeModel
    Nothing -> do
      logCacheGetMissed cacheName key
      return Nothing
