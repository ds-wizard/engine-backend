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
  dtos <- liftIO $ getEvents context
  Scotty.json dtos

postEventsA :: Context -> DSPConfig -> Scotty.ActionM ()
postEventsA context dspConfig = do
  eventsCreateDto <- Scotty.jsonData
  eventsDto <- liftIO $ createEvents context eventsCreateDto
  Scotty.json eventsDto

deleteEventA :: Context -> DSPConfig -> Scotty.ActionM ()
deleteEventA context dspConfig = do
  eventUuid <- Scotty.param "eventUuid"
  isSuccess <- liftIO $ deleteEvent context eventUuid
  if isSuccess
    then Scotty.status noContent204
    else notFoundA
