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
  kmCache <- C.newCache (Just dataExp)
  qcCache <- C.newCache (Just dataExp)
  qriCache <- C.newCache (Just dataExp)
  qwCache <- C.newCache (Just websocketExp)
  uCache <- C.newCache (Just dataExp)
  pCache <- C.newCache (Just dataExp)
  return $
    ServerCache
      { _serverCacheBranchWebsocket = branchCache
      , _serverCacheKnowledgeModel = kmCache
      , _serverCacheQuestionnaireContent = qcCache
      , _serverCacheQuestionnaireReportIndications = qriCache
      , _serverCacheQuestionnaireWebsocket = qwCache
      , _serverCacheUser = uCache
      , _serverCachePackage = pCache
      }

toExpiration hours = fromNanoSecs $ hours * 60 * 60 * 1000 * 1000 * 1000
