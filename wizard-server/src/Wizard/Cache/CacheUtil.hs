module Wizard.Cache.CacheUtil where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Cache as C
import GHC.Int

import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Api.Resource.UserToken.UserTokenJM ()

purgeCache :: AppContextM ()
purgeCache = do
  cache <- asks cache
  liftIO . C.purge $ cache.knowledgeModelEditorWebsocket
  liftIO . C.purge $ cache.projectWebsocket
  liftIO . C.purge $ cache.user
  liftIO . C.purge $ cache.userToken

purgeExpiredCache :: AppContextM ()
purgeExpiredCache = do
  cache <- asks cache
  liftIO . C.purgeExpired $ cache.knowledgeModelEditorWebsocket
  liftIO . C.purgeExpired $ cache.projectWebsocket
  liftIO . C.purgeExpired $ cache.user
  liftIO . C.purgeExpired $ cache.userToken

computeUserCacheSize :: AppContextM String
computeUserCacheSize = computeCacheSize (.user)

computeUserTokenCacheSize :: AppContextM String
computeUserTokenCacheSize = computeCacheSize (.userToken)

computeCacheSize :: ToJSON value => (ServerCache -> C.Cache key value) -> AppContextM String
computeCacheSize cacheAccessor = do
  cache <- asks cache
  items <- liftIO . C.toList . cacheAccessor $ cache
  let values = fmap (\(k, v, exp) -> v) items
  return . formatByteSize . BSL.length . encode $ values

formatByteSize :: Int64 -> String
formatByteSize size
  | size < 1024 = show size ++ " B"
  | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
  | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
  | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"
