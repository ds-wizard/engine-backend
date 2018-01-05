module Service.Event.EventService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resource.Event.EventDTO
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

getEvents :: Context -> String -> IO (Either AppError [EventDTO])
getEvents context branchUuid = do
  eitherBranchWithEvents <- findBranchWithEventsById context branchUuid
  case eitherBranchWithEvents of
    Right branchWithEvents -> return . Right . toDTOs $ branchWithEvents ^. bweEvents
    Left error -> return . Left $ error

createEvents :: Context -> String -> [EventDTO] -> IO (Either AppError [EventDTO])
createEvents context branchUuid eventsCreateDto = do
  eitherBranch <- getBranchById context branchUuid
  case eitherBranch of
    Right _ -> do
      let events = fromDTOs eventsCreateDto
      insertEventsToBranch context branchUuid events
      recompileKnowledgeModel context branchUuid
      return . Right . toDTOs $ events
    Left error -> return . Left $ error

deleteEvents :: Context -> String -> IO (Maybe AppError)
deleteEvents context branchUuid = do
  eitherBranch <- getBranchById context branchUuid
  case eitherBranch of
    Right _ -> do
      deleteEventsAtBranch context branchUuid
      recompileKnowledgeModel context branchUuid
      return Nothing
    Left error -> return . Just $ error
