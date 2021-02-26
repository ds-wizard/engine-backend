module Wizard.Bootstrap.ServerCache where

import Control.Monad.Reader (liftIO)
import qualified Data.Cache as C

import Wizard.Model.Cache.ServerCache
import Wizard.Util.Logger

setupServerCache serverConfig = do
  logInfo _CMP_CACHE "creating server cache"
  cache <- liftIO createServerCache
  logInfo _CMP_CACHE "server cache successfully created"
  return cache

createServerCache = do
  kmCache <- C.newCache Nothing
  qcCache <- C.newCache Nothing
  qriCache <- C.newCache Nothing
  qwCache <- C.newCache Nothing
  uCache <- C.newCache Nothing
  pCache <- C.newCache Nothing
  return $
    ServerCache
      { _serverCacheKnowledgeModel = kmCache
      , _serverCacheQuestionnaireContent = qcCache
      , _serverCacheQuestionnaireReportIndications = qriCache
      , _serverCacheQuestionnaireWebsocket = qwCache
      , _serverCacheUser = uCache
      , _serverCachePackage = pCache
      }
