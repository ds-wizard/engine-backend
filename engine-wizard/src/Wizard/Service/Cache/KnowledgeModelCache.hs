module Wizard.Service.Cache.KnowledgeModelCache where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.String
import Wizard.Model.Context.AppContext
import Wizard.Service.Cache.Common

cacheName = "Knowledge Model"

cacheKey pkgId tagUuids = f' "pkgId: '%s', tagUuids: '%s'" [pkgId, L.intercalate "," (fmap U.toString tagUuids)]

cacheKeyPkgId pkgId = f' "pkgId: '%s'" [pkgId]

cacheKeyTagUuids tagUuids = f' "tagUuids: '%s'" [L.intercalate "," (fmap U.toString tagUuids)]

addToCache :: [Event] -> Maybe String -> [U.UUID] -> KnowledgeModel -> AppContextM ()
addToCache [] (Just pkgId) tagUuids km = do
  let key = cacheKey pkgId tagUuids
  let keyPkgId = cacheKeyPkgId pkgId
  let keyTagUuids = cacheKeyTagUuids tagUuids
  logCacheAddBefore cacheName key
  kmCache <- getCache
  mKmMap <- liftIO $ C.lookup kmCache (H.hash keyPkgId)
  let kmMap = fromMaybe M.empty mKmMap
  let updatedKmMap = M.insert (H.hash keyTagUuids) km kmMap
  liftIO $ C.insert kmCache (H.hash keyPkgId) updatedKmMap
  logCacheAddAfter cacheName key
  return ()
addToCache _ _ _ _ = return ()

getFromCache :: [Event] -> Maybe String -> [U.UUID] -> AppContextM (Maybe KnowledgeModel)
getFromCache [] (Just pkgId) tagUuids = do
  let key = cacheKey pkgId tagUuids
  let keyPkgId = cacheKeyPkgId pkgId
  let keyTagUuids = cacheKeyTagUuids tagUuids
  logCacheGetBefore cacheName key
  kmCache <- getCache
  mKmMap <- liftIO $ C.lookup kmCache (H.hash keyPkgId)
  case mKmMap of
    Just kmMap ->
      case M.lookup (H.hash keyTagUuids) kmMap of
        Just km -> do
          logCacheGetFound cacheName key
          return . Just $ km
        Nothing -> do
          logCacheGetMissed cacheName key
          return Nothing
    Nothing -> do
      logCacheGetMissed cacheName key
      return Nothing
getFromCache _ _ _ = return Nothing

deleteFromCache :: [Event] -> Maybe String -> [U.UUID] -> AppContextM ()
deleteFromCache [] (Just pkgId) tagUuids = do
  let key = cacheKey pkgId tagUuids
  let keyPkgId = cacheKeyPkgId pkgId
  let keyTagUuids = cacheKeyTagUuids tagUuids
  logCacheDeleteBefore cacheName key
  kmCache <- getCache
  mKmMap <- liftIO $ C.lookup kmCache (H.hash keyPkgId)
  let kmMap = fromMaybe M.empty mKmMap
  let updatedKmMap = M.delete (H.hash keyTagUuids) kmMap
  liftIO $ C.insert kmCache (H.hash keyPkgId) updatedKmMap
  logCacheDeleteFinished cacheName key
  return ()
deleteFromCache _ _ _ = return ()

deleteFromCache' :: String -> AppContextM ()
deleteFromCache' pkgId = do
  let key = cacheKeyPkgId pkgId
  logCacheDeleteBefore cacheName key
  kmCache <- getCache
  liftIO $ C.delete kmCache (H.hash key)
  logCacheDeleteFinished cacheName key
  return ()

getCache :: AppContextM (C.Cache Int (M.Map Int KnowledgeModel))
getCache = do
  cache <- asks _appContextCache
  return $ cache ^. knowledgeModel
