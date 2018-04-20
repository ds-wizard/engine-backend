module Api.Handler.Event.EventHandler where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import Web.Scotty.Trans (json, param, status)

import Api.Handler.Common
import Api.Resource.Event.EventDTO
import LensesConfig
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
      eitherUserDto <- liftIO $ createEvents context branchUuid reqDto
      case eitherUserDto of
        Left appError -> sendError appError
        Right userDto -> do
          status created201
          json userDto

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
