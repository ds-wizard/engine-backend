module Wizard.Service.Cache.PackageCache where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H

import LensesConfig
import Shared.Model.Package.Package
import Shared.Util.String
import Wizard.Model.Context.AppContext
import Wizard.Service.Cache.Common

cacheName = "Package"

cacheKey uuid = f' "uuid: '%s'" [uuid]

addToCache :: Package -> AppContextM ()
addToCache record = do
  let key = cacheKey (record ^. pId)
  logCacheAddBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheAddAfter cacheName key
  return ()

getAllFromCache :: AppContextM [Package]
getAllFromCache = do
  cache <- getCache
  records <- liftIO $ C.toList cache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: String -> AppContextM (Maybe Package)
getFromCache uuid = do
  let key = cacheKey uuid
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

updateCache :: Package -> AppContextM ()
updateCache record = do
  let key = cacheKey (record ^. pId)
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

deleteFromCache :: String -> AppContextM ()
deleteFromCache uuid = do
  let key = cacheKey uuid
  logCacheDeleteBefore cacheName key
  cache <- getCache
  liftIO $ C.delete cache (H.hash key)
  logCacheDeleteFinished cacheName key

countCache :: AppContextM Int
countCache = do
  iCache <- getCache
  liftIO $ C.size iCache

getCache :: AppContextM (C.Cache Int Package)
getCache = do
  cache <- asks _appContextCache
  return $ cache ^. package
