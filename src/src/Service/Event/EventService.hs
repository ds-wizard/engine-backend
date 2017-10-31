module Service.Event.EventService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

-- import Api.Resources.Event.EventCreateDTO
import Api.Resources.Event.EventDTO
import Common.Types
import Common.Uuid
import Context
import Database.DAO.Event.EventDAO
import Model.Event.Event
import Service.Event.EventMapper
import Service.Event.EventToDTO

getEvents :: Context -> IO [EventDTO]
getEvents context = do
  events <- findEvents context
  return . toDTOs $ events

createEvents :: Context -> [EventDTO] -> IO [EventDTO]
createEvents context eventsCreateDto = do
  let events = fromDTOs eventsCreateDto
  insertEvents context events
  return $ toDTOs events

-- getEventById :: Context -> String -> IO (Maybe EventDTO)
-- getEventById context eventUuid = do
--     maybeEvent <- findEventById context eventUuid
--     case maybeEvent of
--     Just event -> return . Just $ toDTO event
--     Nothing -> return Nothing
-- modifyEvent :: Context -> String -> EventDTO -> IO (Maybe EventDTO)
-- modifyEvent context eventUuid eventDto = do
--     maybeEvent <- findEventById context eventUuid
--     case maybeEvent of
--     Just event -> do
--         let event = fromEventDTO eventDto (event ^. uUuid) (event ^. uPasswordHash)
--         updateEventById context event
--         return . Just $ eventDto
--     Nothing -> return Nothing
deleteEvent :: Context -> String -> IO Bool
deleteEvent context eventUuid = do
  maybeEvent <- findEventById context eventUuid
  case maybeEvent of
    Just event -> do
      deleteEventById context eventUuid
      return True
    Nothing -> return False
