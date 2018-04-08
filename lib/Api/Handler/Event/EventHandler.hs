module Api.Handler.Event.EventHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import Network.HTTP.Types.Status (created201, noContent204)
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.Event.EventDTO
import Common.Context
import Model.Config.DSWConfig
import Service.Event.EventService

getEventsA :: Context -> DSWConfig -> Scotty.ActionM ()
getEventsA context dswConfig =
  checkPermission context "KM_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    eitherDtos <- liftIO $ getEvents context branchUuid
    case eitherDtos of
      Right dtos -> sendJson dtos
      Left error -> sendError error

postEventsA :: Context -> DSWConfig -> Scotty.ActionM ()
postEventsA context dswConfig =
  checkPermission context "KM_PERM" $ do
    getReqDto $ \reqDto -> do
      branchUuid <- Scotty.param "branchUuid"
      eitherUserDto <- liftIO $ createEvents context branchUuid reqDto
      case eitherUserDto of
        Left appError -> sendError appError
        Right userDto -> do
          Scotty.status created201
          sendJson userDto

deleteEventsA :: Context -> DSWConfig -> Scotty.ActionM ()
deleteEventsA context dswConfig =
  checkPermission context "KM_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    maybeError <- liftIO $ deleteEvents context branchUuid
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
