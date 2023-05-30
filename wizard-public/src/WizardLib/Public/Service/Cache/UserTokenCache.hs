module WizardLib.Public.Service.Cache.UserTokenCache where

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadIO, MonadReader, ask, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Util.String
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.Cache.Common

cacheName = "UserToken"

cacheKey uuid = f' "uuid: '%s'" [uuid]

addToCache :: (MonadIO m, MonadReader s m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken), HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => UserToken -> m ()
addToCache record = do
  let key = cacheKey (U.toString record.uuid)
  logCacheAddBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheAddAfter cacheName key
  return ()

getAllFromCache :: (MonadIO m, MonadReader s m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken), HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => m [UserToken]
getAllFromCache = do
  cache <- getCache
  records <- liftIO $ C.toList cache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: (MonadIO m, MonadReader s m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken), HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> m (Maybe UserToken)
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

updateCache :: (MonadIO m, MonadReader s m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken), HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => UserToken -> m ()
updateCache record = do
  let key = cacheKey (U.toString record.uuid)
  logCacheModifyBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheModifyAfter cacheName key
  return ()

deleteAllFromCache :: (MonadIO m, MonadReader s m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken), HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => m ()
deleteAllFromCache = do
  logCacheDeleteAllBefore cacheName
  cache <- getCache
  liftIO $ C.purge cache
  logCacheDeleteAllFinished cacheName

deleteFromCache :: (MonadIO m, MonadReader s m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken), HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => String -> m ()
deleteFromCache uuid = do
  let key = cacheKey uuid
  logCacheDeleteBefore cacheName key
  cache <- getCache
  liftIO $ C.delete cache (H.hash key)
  logCacheDeleteFinished cacheName key

countCache :: (MonadIO m, MonadReader s m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken), HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => m Int
countCache = do
  iCache <- getCache
  liftIO $ C.size iCache

getCache :: (MonadIO m, MonadReader s m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken), HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, MonadLogger m) => m (C.Cache Int UserToken)
getCache = do
  context <- ask
  let cache = context.cache'
  return $ cache.userToken
