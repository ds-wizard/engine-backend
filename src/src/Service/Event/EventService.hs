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
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import KMMigration.Migration.Migration
import Model.Event.Event
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Service.Event.EventMapper
import Service.Event.EventToDTO
import Service.KnowledgeModel.KnowledgeModelService
import Service.KnowledgeModelContainer.KnowledgeModelContainerService

getEvents :: Context -> String -> IO (Maybe [EventDTO])
getEvents context kmcUuid = do
  maybeKmcWithEvents <- findKmcWithEventsById context kmcUuid
  case maybeKmcWithEvents of
    Just kmcWithEvents -> return . Just . toDTOs $ kmcWithEvents ^. kmcweEvents
    _ -> return Nothing

createEvents :: Context -> String -> [EventDTO] -> IO (Maybe [EventDTO])
createEvents context kmcUuid eventsCreateDto = do
  maybeKmc <- getKnowledgeModelContainerById context kmcUuid
  case maybeKmc of
    Just kmc -> do
      let events = fromDTOs eventsCreateDto
      insertEventsToKmc context kmcUuid events
      recompileKnowledgeModel context kmcUuid
      return . Just . toDTOs $ events
    Nothing -> return Nothing

recompileKnowledgeModel :: Context -> String -> IO ()
recompileKnowledgeModel context kmcUuid = do
  maybeKmc <- findKmcWithEventsById context kmcUuid
  case maybeKmc of
    Just kmc -> do
      let events = kmc ^. kmcweEvents
      let newKM = migrate undefined events
      updateKnowledgeModelByKmcId context kmcUuid newKM
    _ -> return ()

deleteEvents :: Context -> String -> IO Bool
deleteEvents context kmcUuid = do
  maybeKmc <- getKnowledgeModelContainerById context kmcUuid
  case maybeKmc of
    Just kmc -> do
      deleteEventAtKmc context kmcUuid
      return True
    Nothing -> return False
