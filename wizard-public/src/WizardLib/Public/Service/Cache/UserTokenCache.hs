module WizardLib.Public.Service.Cache.UserTokenCache where

import Control.Monad.Reader (ask, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.Cache.Common

cacheName = "UserToken"

cacheKey uuid = f' "uuid: '%s'" [uuid]

addToCache :: (AppContextC s sc m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken)) => UserToken -> m ()
addToCache record = do
  let key = cacheKey (U.toString record.uuid)
  logCacheAddBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheAddAfter cacheName key
  return ()

getAllFromCache :: (AppContextC s sc m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken)) => m [UserToken]
getAllFromCache = do
  cache <- getCache
  records <- liftIO $ C.toList cache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: (AppContextC s sc m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken)) => String -> m (Maybe UserToken)
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

updateCache :: (AppContextC s sc m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken)) => UserToken -> m ()
updateCache record = do
  let key = cacheKey (U.toString record.uuid)
  logCacheModifyBefore cacheName key
  cache <- getCache
  liftIO $ C.insert cache (H.hash key) record
  logCacheModifyAfter cacheName key
  return ()

deleteAllFromCache :: (AppContextC s sc m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken)) => m ()
deleteAllFromCache = do
  logCacheDeleteAllBefore cacheName
  cache <- getCache
  liftIO $ C.purge cache
  logCacheDeleteAllFinished cacheName

deleteFromCache :: (AppContextC s sc m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken)) => String -> m ()
deleteFromCache uuid = do
  let key = cacheKey uuid
  logCacheDeleteBefore cacheName key
  cache <- getCache
  liftIO $ C.delete cache (H.hash key)
  logCacheDeleteFinished cacheName key

countCache :: (AppContextC s sc m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken)) => m Int
countCache = do
  iCache <- getCache
  liftIO $ C.size iCache

getCache :: (AppContextC s sc m, HasField "cache'" s serverCache, HasField "userToken" serverCache (C.Cache Int UserToken)) => m (C.Cache Int UserToken)
getCache = do
  context <- ask
  let cache = context.cache'
  return $ cache.userToken
