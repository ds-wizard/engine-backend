module Wizard.Cache.ProjectWebsocketCache where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Websocket.WebsocketRecord
import WizardLib.Public.Service.Cache.Common

cacheName = "Project Websocket"

cacheKey connectionUuid = f' "connection: '%s'" [U.toString connectionUuid]

addToCache :: WebsocketRecord -> AppContextM ()
addToCache record = do
  let key = cacheKey record.connectionUuid
  logCacheAddBefore cacheName key
  projectCache <- getCache
  liftIO $ C.insert projectCache (H.hash key) record
  logCacheAddAfter cacheName key
  return ()

getAllFromCache :: AppContextM [WebsocketRecord]
getAllFromCache = do
  projectCache <- getCache
  records <- liftIO $ C.toList projectCache
  return . fmap (\(_, v, _) -> v) $ records

getFromCache :: U.UUID -> AppContextM (Maybe WebsocketRecord)
getFromCache connectionUuid = do
  let key = cacheKey connectionUuid
  logCacheGetBefore cacheName key
  projectCache <- getCache
  mValue <- liftIO $ C.lookup projectCache (H.hash key)
  case mValue of
    Just value -> do
      logCacheGetFound cacheName key
      return . Just $ value
    Nothing -> do
      logCacheGetMissed cacheName key
      return Nothing

updateCache :: WebsocketRecord -> AppContextM ()
updateCache record = do
  let key = cacheKey record.connectionUuid
  logCacheModifyBefore cacheName key
  projectCache <- getCache
  liftIO $ C.insert projectCache (H.hash key) record
  logCacheModifyAfter cacheName key
  return ()

getFromCache' :: U.UUID -> AppContextM WebsocketRecord
getFromCache' connectionUuid = do
  mRecord <- getFromCache connectionUuid
  case mRecord of
    Just record -> return record
    Nothing -> throwError . NotExistsError . _ERROR_API__WEBSOCKET_RECORD_NOT_FOUND $ U.toString connectionUuid

deleteFromCache :: U.UUID -> AppContextM ()
deleteFromCache connectionUuid = do
  let key = cacheKey connectionUuid
  logCacheDeleteBefore cacheName key
  projectCache <- getCache
  liftIO $ C.delete projectCache (H.hash key)
  logCacheDeleteFinished cacheName key

countCache :: AppContextM Int
countCache = do
  iCache <- getCache
  liftIO $ C.size iCache

getCache :: AppContextM (C.Cache Int WebsocketRecord)
getCache = do
  cache <- asks cache
  return $ cache.projectWebsocket
