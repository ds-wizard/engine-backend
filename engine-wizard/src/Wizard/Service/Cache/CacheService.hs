module Wizard.Service.Cache.CacheService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C

import LensesConfig
import Wizard.Model.Context.AppContext

purgeCache :: AppContextM ()
purgeCache = do
  cache <- asks _appContextCache
  liftIO . C.purge $ cache ^. branchWebsocket
  liftIO . C.purge $ cache ^. questionnaireWebsocket
  liftIO . C.purge $ cache ^. user
  liftIO . C.purge $ cache ^. userToken

purgeExpiredCache :: AppContextM ()
purgeExpiredCache = do
  cache <- asks _appContextCache
  liftIO . C.purgeExpired $ cache ^. branchWebsocket
  liftIO . C.purgeExpired $ cache ^. questionnaireWebsocket
  liftIO . C.purgeExpired $ cache ^. user
  liftIO . C.purgeExpired $ cache ^. userToken
