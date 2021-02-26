module Wizard.Service.Cache.QuestionnaireContentCache where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Cache as C
import qualified Data.Hashable as H
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Util.String
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Service.Cache.Common

cacheName = "Questionnaire Content"

cacheKey lastEventUuid = f' "lastEventUuuid: '%s'" [U.toString lastEventUuid]

addToCache :: [QuestionnaireEvent] -> QuestionnaireContent -> AppContextM ()
addToCache qtnEvents qtnContent =
  if null qtnEvents
    then return ()
    else do
      let lastEvent = last qtnEvents
      let key = cacheKey (lastEvent ^. uuid')
      logCacheAddBefore cacheName key
      aCache <- getCache
      liftIO $ C.insert aCache (H.hash key) qtnContent
      logCacheAddAfter cacheName key
      return ()

getFromCache :: [QuestionnaireEvent] -> AppContextM (Maybe QuestionnaireContent)
getFromCache qtnEvents =
  if null qtnEvents
    then return Nothing
    else do
      let lastEvent = last qtnEvents
      let key = cacheKey (lastEvent ^. uuid')
      logCacheGetBefore cacheName key
      aCache <- getCache
      mRecord <- liftIO $ C.lookup aCache (H.hash key)
      case mRecord of
        Just record -> do
          logCacheGetFound cacheName key
          return . Just $ record
        Nothing -> do
          logCacheGetMissed cacheName key
          return Nothing

getCache :: AppContextM (C.Cache Int QuestionnaireContent)
getCache = do
  cache <- asks _appContextCache
  return $ cache ^. questionnaireContent
