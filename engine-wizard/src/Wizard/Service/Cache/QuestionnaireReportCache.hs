module Wizard.Service.Cache.QuestionnaireReportCache where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.String
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Report.Report
import Wizard.Service.Cache.Common

cacheName = "Report"

cacheKey qtnUuid repliesHash = f' "qtnUuid: '%s', repliesHash: '%s'" [U.toString qtnUuid, show repliesHash]

addToCache :: Questionnaire -> QuestionnaireContent -> [Indication] -> AppContextM ()
addToCache qtn qtnCtn indications = do
  let qtnUuid = qtn ^. uuid
  let repliesHash = H.hash . M.toList $ qtnCtn ^. replies
  let key = cacheKey qtnUuid repliesHash
  logCacheAddBefore cacheName key
  iCache <- getCache
  liftIO $ C.insert iCache (H.hash key) indications
  logCacheAddAfter cacheName key
  return ()

getFromCache :: Questionnaire -> QuestionnaireContent -> AppContextM (Maybe [Indication])
getFromCache qtn qtnCtn = do
  let qtnUuid = qtn ^. uuid
  let repliesHash = H.hash . M.toList $ qtnCtn ^. replies
  let key = cacheKey qtnUuid repliesHash
  logCacheGetBefore cacheName key
  iCache <- getCache
  mIs <- liftIO $ C.lookup iCache (H.hash key)
  case mIs of
    Just is -> do
      logCacheGetFound cacheName key
      return . Just $ is
    Nothing -> do
      logCacheGetMissed cacheName key
      return Nothing

getCache :: AppContextM (C.Cache Int [Indication])
getCache = do
  cache <- asks _appContextCache
  return $ cache ^. questionnaireReportIndications
