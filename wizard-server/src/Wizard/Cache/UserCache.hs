module Wizard.Cache.UserCache where

import Control.Monad (when)
import Control.Monad.Reader (ask, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.UUID as U

import Shared.Common.Util.String
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.User
import WizardLib.Public.Service.Cache.Common

cacheName = "User"

cacheKey uuid tenantUuid = f' "uuid: '%s', tenantUuid: '%s'" [uuid, tenantUuid]

addToCache :: User -> AppContextM ()
addToCache record = do
  context <- ask
  when
    context.serverConfig.cache.dataEnabled
    ( do
        let key = cacheKey (U.toString record.uuid) (U.toString record.tenantUuid)
        logCacheAddBefore cacheName key
        cache <- getCache
        liftIO $ C.insert cache (H.hash key) record
        logCacheAddAfter cacheName key
        return ()
    )

getAllFromCache :: AppContextM [User]
getAllFromCache = do
  cache <- getCache
  records <- liftIO $ C.toList cache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: (String, String) -> AppContextM (Maybe User)
getFromCache (uuid, tenantUuid) = do
  context <- ask
  if context.serverConfig.cache.dataEnabled
    then do
      let key = cacheKey uuid tenantUuid
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
    else return Nothing

updateCache :: User -> AppContextM ()
updateCache record = do
  context <- ask
  when
    context.serverConfig.cache.dataEnabled
    ( do
        let key = cacheKey (U.toString record.uuid) (U.toString record.tenantUuid)
        logCacheModifyBefore cacheName key
        cache <- getCache
        liftIO $ C.insert cache (H.hash key) record
        logCacheModifyAfter cacheName key
        return ()
    )

deleteAllFromCache :: AppContextM ()
deleteAllFromCache = do
  logCacheDeleteAllBefore cacheName
  cache <- getCache
  liftIO $ C.purge cache
  logCacheDeleteAllFinished cacheName

deleteFromCache :: (String, String) -> AppContextM ()
deleteFromCache (uuid, tenantUuid) = do
  context <- ask
  when
    context.serverConfig.cache.dataEnabled
    ( do
        let key = cacheKey uuid tenantUuid
        logCacheDeleteBefore cacheName key
        cache <- getCache
        liftIO $ C.delete cache (H.hash key)
        logCacheDeleteFinished cacheName key
    )

countCache :: AppContextM Int
countCache = do
  iCache <- getCache
  liftIO $ C.size iCache

getCache :: AppContextM (C.Cache Int User)
getCache = do
  context <- ask
  return $ context.cache.user
