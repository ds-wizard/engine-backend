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
import Context
import DSPConfig
import Service.Event.EventService

getEventsA :: Context -> DSPConfig -> Scotty.ActionM ()
getEventsA context dspConfig = do
  kmcUuid <- Scotty.param "kmcUuid"
  maybeDtos <- liftIO $ getEvents context kmcUuid
  case maybeDtos of
    Just dtos -> sendJson dtos
    _ -> notFoundA

postEventsA :: Context -> DSPConfig -> Scotty.ActionM ()
postEventsA context dspConfig =
  getReqDto $ \reqDto -> do
    kmcUuid <- Scotty.param "kmcUuid"
    maybeEventsDto <- liftIO $ createEvents context kmcUuid reqDto
    case maybeEventsDto of
      Just eventsDto -> do
        Scotty.status created201
        sendJson eventsDto
      _ -> notFoundA

deleteEventsA :: Context -> DSPConfig -> Scotty.ActionM ()
deleteEventsA context dspConfig = do
  kmcUuid <- Scotty.param "kmcUuid"
  isSuccess <- liftIO $ deleteEvents context kmcUuid
  if isSuccess
    then Scotty.status noContent204
    else notFoundA
