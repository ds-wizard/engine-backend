module Wizard.Bootstrap.ServerCache where

import Control.Monad.Reader (liftIO)
import qualified Data.Cache as C
import System.Clock

import Shared.Common.Util.Logger
import Wizard.Model.Cache.ServerCache

setupServerCache serverConfig = do
  logInfo _CMP_CACHE "creating server cache"
  cache <- liftIO (createServerCache serverConfig)
  logInfo _CMP_CACHE "server cache successfully created"
  return cache

createServerCache serverConfig = do
  let dataExp = toExpiration (serverConfig.cache.dataExpiration)
  let websocketExp = toExpiration (serverConfig.cache.websocketExpiration)
  branchCache <- C.newCache (Just websocketExp)
  qwCache <- C.newCache (Just websocketExp)
  uCache <- C.newCache (Just dataExp)
  uTokenCache <- C.newCache (Just dataExp)
  return $
    ServerCache
      { branchWebsocket = branchCache
      , questionnaireWebsocket = qwCache
      , user = uCache
      , userToken = uTokenCache
      }

toExpiration hours = fromNanoSecs $ hours * 60 * 60 * 1000 * 1000 * 1000
