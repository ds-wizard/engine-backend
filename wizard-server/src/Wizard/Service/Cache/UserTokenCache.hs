module Wizard.Service.Cache.UserTokenCache where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.UUID as U

import Shared.Common.Util.String
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.AppContext
import Wizard.Model.User.UserToken
import Wizard.Service.Cache.Common

cacheName = "UserToken"

cacheKey uuid = f' "uuid: '%s'" [uuid]

addToCache :: UserToken -> AppContextM ()
addToCache record = do
  let key = cacheKey (U.toString record.uuid)
  logCacheAddBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheAddAfter cacheName key
  return ()

getAllFromCache :: AppContextM [UserToken]
getAllFromCache = do
  cache <- getCache
  records <- liftIO $ C.toList cache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: String -> AppContextM (Maybe UserToken)
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

updateCache :: UserToken -> AppContextM ()
updateCache record = do
  let key = cacheKey (U.toString record.uuid)
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

getCache :: AppContextM (C.Cache Int UserToken)
getCache = do
  cache <- asks cache
  return $ cache.userToken
