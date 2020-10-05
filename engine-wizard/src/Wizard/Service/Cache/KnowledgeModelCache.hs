module Wizard.Service.Cache.KnowledgeModelCache where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.List as L
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.String
import Wizard.Model.Context.AppContext
import Wizard.Service.Cache.Common

cacheName = "Knowledge Model"

cacheKey pkgId tagUuids = f' "pkgId: '%s', tagUuids: '%s'" [pkgId, L.intercalate "," (fmap U.toString tagUuids)]

addToCache :: [Event] -> Maybe String -> [U.UUID] -> KnowledgeModel -> AppContextM ()
addToCache [] (Just pkgId) tagUuids km = do
  let key = cacheKey pkgId tagUuids
  logCacheAddBefore cacheName key
  kmCache <- getCache
  liftIO $ C.insert kmCache (H.hash key) km
  logCacheAddAfter cacheName key
  return ()
addToCache _ _ _ _ = return ()

getFromCache :: [Event] -> Maybe String -> [U.UUID] -> AppContextM (Maybe KnowledgeModel)
getFromCache [] (Just pkgId) tagUuids = do
  let key = cacheKey pkgId tagUuids
  logCacheGetBefore cacheName key
  kmCache <- getCache
  mKm <- liftIO $ C.lookup kmCache (H.hash key)
  case mKm of
    Just km -> do
      logCacheGetFound cacheName key
      return . Just $ km
    Nothing -> do
      logCacheGetMissed cacheName key
      return Nothing
getFromCache _ _ _ = return Nothing

getCache :: AppContextM (C.Cache Int KnowledgeModel)
getCache = do
  cache <- asks _appContextCache
  return $ cache ^. knowledgeModel
