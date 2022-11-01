module Wizard.Bootstrap.ServerCache where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import qualified Data.Cache as C
import System.Clock

import LensesConfig
import Wizard.Model.Cache.ServerCache
import Wizard.Util.Logger

setupServerCache serverConfig = do
  logInfo _CMP_CACHE "creating server cache"
  cache <- liftIO (createServerCache serverConfig)
  logInfo _CMP_CACHE "server cache successfully created"
  return cache

createServerCache serverConfig = do
  let dataExp = toExpiration (serverConfig ^. cache . dataExpiration)
  let websocketExp = toExpiration (serverConfig ^. cache . websocketExpiration)
  branchCache <- C.newCache (Just websocketExp)
  qwCache <- C.newCache (Just websocketExp)
  uCache <- C.newCache (Just dataExp)
  uTokenCache <- C.newCache (Just dataExp)
  return $
    ServerCache
      { _serverCacheBranchWebsocket = branchCache
      , _serverCacheQuestionnaireWebsocket = qwCache
      , _serverCacheUser = uCache
      , _serverCacheUserToken = uTokenCache
      }

toExpiration hours = fromNanoSecs $ hours * 60 * 60 * 1000 * 1000 * 1000
