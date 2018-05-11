module Api.Handler.Event.EventHandler where

import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Event.EventDTO ()
import Model.Context.AppContext
import Service.Event.EventService

getEventsA :: Endpoint
getEventsA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $ do
    branchUuid <- param "branchUuid"
    eitherDtos <- liftIO $ getEvents context branchUuid
    case eitherDtos of
      Right dtos -> json dtos
      Left error -> sendError error

postEventsA :: Endpoint
postEventsA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $ do
    getReqDto $ \reqDto -> do
      branchUuid <- param "branchUuid"
      eitherEventsDto <- liftIO $ createEvents context branchUuid reqDto
      case eitherEventsDto of
        Left appError -> sendError appError
        Right eventsDto -> do
          status created201
          json eventsDto

deleteEventsA :: Endpoint
deleteEventsA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  checkPermission context "KM_PERM" $ do
    branchUuid <- param "branchUuid"
    maybeError <- liftIO $ deleteEvents context branchUuid
    case maybeError of
      Nothing -> status noContent204
      Just error -> sendError error
