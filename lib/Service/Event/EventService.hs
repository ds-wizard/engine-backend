module Service.Event.EventService where

import Control.Lens ((^.))

import Api.Resource.Event.EventDTO
import Common.Context
import Common.Error
import Database.DAO.Event.EventDAO
import Model.Branch.Branch
import Service.Branch.BranchService
import Service.Event.EventMapper
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
      result <- recompileKnowledgeModel context branchUuid
      case result of
        Right km -> return . Right . toDTOs $ events
        Left error -> return . Left $ error
    Left error -> return . Left $ error

deleteEvents :: Context -> String -> IO (Maybe AppError)
deleteEvents context branchUuid = do
  eitherBranch <- getBranchById context branchUuid
  case eitherBranch of
    Right _ -> do
      deleteEventsAtBranch context branchUuid
      result <- recompileKnowledgeModel context branchUuid
      case result of
        Right km -> return Nothing
        Left error -> return . Just $ error
    Left error -> return . Just $ error
