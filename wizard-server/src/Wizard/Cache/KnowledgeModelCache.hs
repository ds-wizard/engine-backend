module Wizard.Cache.KnowledgeModelCache where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import Data.Hashable
import qualified Data.Hashable as H
import qualified Data.UUID as U

import Shared.Common.Util.String
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.Public.Service.Cache.Common

cacheName = "KnowledgeModel"

cacheKey :: String -> [U.UUID] -> U.UUID -> String
cacheKey packageId tagUuids tenantUuid =
  f''
    "packageId: '${packageId}', tagUuids: '${tagUuids}', tenantUuid: '${tenantUuid}'"
    [ ("packageId", show . hash $ packageId)
    , ("tagUuids", show . hash $ tagUuids)
    , ("tenantUuid", show . hash $ tenantUuid)
    ]

addToCache :: (String, [U.UUID], U.UUID) -> KnowledgeModel -> AppContextM ()
addToCache (packageId, tagUuids, tenantUuid) record = do
  let key = cacheKey packageId tagUuids tenantUuid
  logCacheAddBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheAddAfter cacheName key
  return ()

getAllFromCache :: AppContextM [KnowledgeModel]
getAllFromCache = do
  cache <- getCache
  records <- liftIO $ C.toList cache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: (String, [U.UUID], U.UUID) -> AppContextM (Maybe KnowledgeModel)
getFromCache (packageId, tagUuids, tenantUuid) = do
  let key = cacheKey packageId tagUuids tenantUuid
  logCacheGetBefore cacheName key
  cache <- getCache
  mValue <- liftIO $ C.lookup cache (H.hash key)
  case mValue of
    Just value -> do
      logCacheGetFound cacheName key
      return . Just $ value
    Nothing -> do
      logCacheGetMissed cacheName key
      return Nothing

updateCache :: (String, [U.UUID], U.UUID) -> KnowledgeModel -> AppContextM ()
updateCache (packageId, tagUuids, tenantUuid) record = do
  let key = cacheKey packageId tagUuids tenantUuid
  logCacheModifyBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheModifyAfter cacheName key
  return ()

deleteAllFromCache :: AppContextM ()
deleteAllFromCache = do
  logCacheDeleteAllBefore cacheName
  cache <- getCache
  liftIO $ C.purge cache
  logCacheDeleteAllFinished cacheName

deleteFromCache :: (String, [U.UUID], U.UUID) -> AppContextM ()
deleteFromCache (packageId, tagUuids, tenantUuid) = do
  let key = cacheKey packageId tagUuids tenantUuid
  logCacheDeleteBefore cacheName key
  cache <- getCache
  liftIO $ C.delete cache (H.hash key)
  logCacheDeleteFinished cacheName key

countCache :: AppContextM Int
countCache = do
  iCache <- getCache
  liftIO $ C.size iCache

getCache :: AppContextM (C.Cache Int KnowledgeModel)
getCache = do
  cache <- asks cache
  return $ cache.knowledgeModel
