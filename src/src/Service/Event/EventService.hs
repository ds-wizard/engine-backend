module Service.Event.EventService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resources.Event.EventDTO
import Common.Error
import Common.Types
import Common.Uuid
import Common.Context
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.Package.PackageDAO
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Model.Package.Package
import Service.Event.EventMapper
import Service.Event.EventToDTO
import Service.KnowledgeModel.KnowledgeModelService
import Service.KnowledgeModelContainer.KnowledgeModelContainerService
import Service.Migrator.Migrator

getEvents :: Context -> String -> IO (Either AppError [EventDTO])
getEvents context kmcUuid = do
  eitherKmcWithEvents <- findKmcWithEventsById context kmcUuid
  case eitherKmcWithEvents of
    Right kmcWithEvents ->
      return . Right . toDTOs $ kmcWithEvents ^. kmcweEvents
    Left error -> return . Left $ error

getEventsFromPackage :: Context -> String -> IO (Either AppError [Event])
getEventsFromPackage context pkgId = do
  eitherPackage <- findPackageWithEventsById context pkgId
  case eitherPackage of
    Right package -> return . Right . getAllEventsFromPackage $ package
    Left error -> return . Left $ error

createEvents :: Context
             -> String
             -> [EventDTO]
             -> IO (Either AppError [EventDTO])
createEvents context kmcUuid eventsCreateDto = do
  eitherKmc <- getKnowledgeModelContainerById context kmcUuid
  case eitherKmc of
    Right kmc -> do
      let events = fromDTOs eventsCreateDto
      insertEventsToKmc context kmcUuid events
      recompileKnowledgeModel context kmcUuid
      return . Right . toDTOs $ events
    Left error -> return . Left $ error

recompileKnowledgeModel :: Context
                        -> String
                        -> IO (Either AppError KnowledgeModel)
recompileKnowledgeModel context kmcUuid = do
  eitherKmc <- findKmcWithEventsById context kmcUuid
  case eitherKmc of
    Right kmc -> do
      let mPpId = kmc ^. kmcweParentPackageId
      case mPpId of
        Just ppId -> do
          eitherEventsFromPackage <- getEventsFromPackage context ppId
          case eitherEventsFromPackage of
            Right eventsFromPackage -> do
              let eventsFromKM = kmc ^. kmcweEvents
              let events = eventsFromPackage ++ eventsFromKM
              let eitherNewKM = migrate Nothing events
              case eitherNewKM of
                Right newKM -> do
                  updateKnowledgeModelByKmcId context kmcUuid newKM
                  return . Right $ newKM
                Left error -> return . Left $ error
            Left error -> return . Left $ error
        Nothing -> do
          let events = kmc ^. kmcweEvents
          let eitherNewKM = migrate Nothing events
          case eitherNewKM of
            Right newKM -> do
              updateKnowledgeModelByKmcId context kmcUuid newKM
              return . Right $ newKM
            Left error -> return . Left $ error
    Left error -> return . Left $ error

deleteEvents :: Context -> String -> IO (Maybe AppError)
deleteEvents context kmcUuid = do
  eitherKmc <- getKnowledgeModelContainerById context kmcUuid
  case eitherKmc of
    Right kmc -> do
      deleteEventAtKmc context kmcUuid
      recompileKnowledgeModel context kmcUuid
      return Nothing
    Left error -> return . Just $ error

getAllEventsFromPackage :: PackageWithEvents -> [Event]
getAllEventsFromPackage package =
  eventsFromParentPackage ++ package ^. pkgweEvents
  where
    eventsFromParentPackage =
      case package ^. pkgweParentPackage of
        Just parentPackage -> getAllEventsFromPackage parentPackage
        Nothing -> []
