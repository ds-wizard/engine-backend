module Wizard.Service.Cache.UserCache where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.String
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.Cache.Common

cacheName = "User"

cacheKey uuid = f' "uuid: '%s'" [uuid]

addToCache :: User -> AppContextM ()
addToCache record = do
  let key = cacheKey (U.toString $ record ^. uuid)
  logCacheAddBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheAddAfter cacheName key
  return ()

getAllFromCache :: AppContextM [User]
getAllFromCache = do
  cache <- getCache
  records <- liftIO $ C.toList cache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: String -> AppContextM (Maybe User)
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

updateCache :: User -> AppContextM ()
updateCache record = do
  let key = cacheKey (U.toString $ record ^. uuid)
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

getCache :: AppContextM (C.Cache Int User)
getCache = do
  cache <- asks _appContextCache
  return $ cache ^. user
