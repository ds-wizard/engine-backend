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
import Database.DAO.Package.PackageDAO
import KMMigration.Migration.Migration
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Model.Package.Package
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

getEventsFromPackage :: Context -> String -> String -> IO (Maybe [Event])
getEventsFromPackage context pName pVersion = do
  maybePackage <- findPackageWithEventsByNameAndVersion context pName pVersion
  case maybePackage of
    Just package -> return . Just . getAllEventsFromPackage $ package
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

recompileKnowledgeModel :: Context -> String -> IO (Maybe KnowledgeModel)
recompileKnowledgeModel context kmcUuid = do
  maybeKmc <- findKmcWithEventsById context kmcUuid
  case maybeKmc of
    Just kmc -> do
      let ppName = kmc ^. kmcweParentPackageName
      let ppVersion = kmc ^. kmcweParentPackageVersion
      maybeEventsFromPackage <- getEventsFromPackage context ppName ppVersion
      case maybeEventsFromPackage of
        Just eventsFromPackage -> do
          let eventsFromKM = kmc ^. kmcweEvents
          let events = eventsFromPackage ++ eventsFromKM
          let newKM = migrate Nothing events
          updateKnowledgeModelByKmcId context kmcUuid newKM
          return . Just $ newKM
        _ -> return Nothing
    _ -> return Nothing

deleteEvents :: Context -> String -> IO Bool
deleteEvents context kmcUuid = do
  maybeKmc <- getKnowledgeModelContainerById context kmcUuid
  case maybeKmc of
    Just kmc -> do
      deleteEventAtKmc context kmcUuid
      return True
    Nothing -> return False

getAllEventsFromPackage :: PackageWithEvents -> [Event]
getAllEventsFromPackage package =
  eventsFromParentPackage ++ package ^. pkgweEvents
  where
    eventsFromParentPackage =
      case package ^. pkgweParentPackage of
        Just parentPackage -> getAllEventsFromPackage parentPackage
        Nothing -> []
