module Wizard.Service.Cache.CacheService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C

import LensesConfig
import Wizard.Model.Context.AppContext

purgeCache :: AppContextM ()
purgeCache = do
  cache <- asks _appContextCache
  liftIO . C.purge $ cache ^. knowledgeModel
  liftIO . C.purge $ cache ^. questionnaireContent
  liftIO . C.purge $ cache ^. questionnaireReportIndications
  liftIO . C.purge $ cache ^. questionnaireWebsocket
  liftIO . C.purge $ cache ^. user
  liftIO . C.purge $ cache ^. package
  return ()
