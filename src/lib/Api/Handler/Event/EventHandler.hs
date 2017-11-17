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
import Api.Resources.Event.EventDTO
import Common.Context
import Common.DSPConfig
import Service.Event.EventService

getEventsA :: Context -> DSPConfig -> Scotty.ActionM ()
getEventsA context dspConfig =
  checkPermission context "KM_PERM" $ do
    kmcUuid <- Scotty.param "kmcUuid"
    eitherDtos <- liftIO $ getEvents context kmcUuid
    case eitherDtos of
      Right dtos -> sendJson dtos
      Left error -> sendError error

postEventsA :: Context -> DSPConfig -> Scotty.ActionM ()
postEventsA context dspConfig =
  checkPermission context "KM_PERM" $ do
    getReqDto $ \reqDto -> do
      kmcUuid <- Scotty.param "kmcUuid"
      eitherUserDto <- liftIO $ createEvents context kmcUuid reqDto
      case eitherUserDto of
        Left appError -> sendError appError
        Right userDto -> do
          Scotty.status created201
          sendJson userDto

deleteEventsA :: Context -> DSPConfig -> Scotty.ActionM ()
deleteEventsA context dspConfig =
  checkPermission context "KM_PERM" $ do
    kmcUuid <- Scotty.param "kmcUuid"
    maybeError <- liftIO $ deleteEvents context kmcUuid
    case maybeError of
      Nothing -> Scotty.status noContent204
      Just error -> sendError error
