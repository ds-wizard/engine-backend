module Service.Event.EventService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resources.Event.EventDTO
import Common.Context
import Common.Error
import Common.Types
import Common.Uuid
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.Package.PackageDAO
import Model.Branch.Branch
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Package.Package
import Service.Branch.BranchService
import Service.Event.EventMapper
import Service.Event.EventToDTO
import Service.KnowledgeModel.KnowledgeModelService
import Service.Migrator.Applicator

getEvents :: Context -> String -> IO (Either AppError [EventDTO])
getEvents context branchUuid = do
  eitherBranchWithEvents <- findBranchWithEventsById context branchUuid
  case eitherBranchWithEvents of
    Right branchWithEvents -> return . Right . toDTOs $ branchWithEvents ^. bweEvents
    Left error -> return . Left $ error

getEventsFromPackage :: Context -> String -> IO (Either AppError [Event])
getEventsFromPackage context pkgId = do
  eitherPackage <- findPackageWithEventsById context pkgId
  case eitherPackage of
    Right package -> return . Right . getAllEventsFromPackage $ package
    Left error -> return . Left $ error

createEvents :: Context -> String -> [EventDTO] -> IO (Either AppError [EventDTO])
createEvents context branchUuid eventsCreateDto = do
  eitherBranch <- getBranchById context branchUuid
  case eitherBranch of
    Right branch -> do
      let events = fromDTOs eventsCreateDto
      insertEventsToBranch context branchUuid events
      recompileKnowledgeModel context branchUuid
      return . Right . toDTOs $ events
    Left error -> return . Left $ error

recompileKnowledgeModel :: Context -> String -> IO (Either AppError KnowledgeModel)
recompileKnowledgeModel context branchUuid = do
  eitherBranch <- findBranchWithEventsById context branchUuid
  case eitherBranch of
    Right branch -> do
      let mPpId = branch ^. bweParentPackageId
      case mPpId of
        Just ppId -> do
          eitherEventsFromPackage <- getEventsFromPackage context ppId
          case eitherEventsFromPackage of
            Right eventsFromPackage -> do
              let eventsFromKM = branch ^. bweEvents
              let events = eventsFromPackage ++ eventsFromKM
              let eitherNewKM = runApplicator Nothing events
              case eitherNewKM of
                Right newKM -> do
                  updateKnowledgeModelByBranchId context branchUuid newKM
                  return . Right $ newKM
                Left error -> return . Left $ error
            Left error -> return . Left $ error
        Nothing -> do
          let events = branch ^. bweEvents
          let eitherNewKM = runApplicator Nothing events
          case eitherNewKM of
            Right newKM -> do
              updateKnowledgeModelByBranchId context branchUuid newKM
              return . Right $ newKM
            Left error -> return . Left $ error
    Left error -> return . Left $ error

deleteEvents :: Context -> String -> IO (Maybe AppError)
deleteEvents context branchUuid = do
  eitherBranch <- getBranchById context branchUuid
  case eitherBranch of
    Right branch -> do
      deleteEventAtBranch context branchUuid
      recompileKnowledgeModel context branchUuid
      return Nothing
    Left error -> return . Just $ error

getAllEventsFromPackage :: PackageWithEvents -> [Event]
getAllEventsFromPackage package = eventsFromParentPackage ++ package ^. pkgweEvents
  where
    eventsFromParentPackage =
      case package ^. pkgweParentPackage of
        Just parentPackage -> getAllEventsFromPackage parentPackage
        Nothing -> []
