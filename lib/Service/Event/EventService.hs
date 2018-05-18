module Service.Event.EventService where

import Control.Lens ((^.))

import Api.Resource.Event.EventDTO
import Common.Error
import Database.DAO.Event.EventDAO
import Model.Branch.Branch
import Model.Context.AppContext
import Service.Branch.BranchService
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelService

getEvents :: String -> AppContextM (Either AppError [EventDTO])
getEvents branchUuid = do
  eitherBranchWithEvents <- findBranchWithEventsById branchUuid
  case eitherBranchWithEvents of
    Right branchWithEvents -> return . Right . toDTOs $ branchWithEvents ^. bweEvents
    Left error -> return . Left $ error

createEvents :: String -> [EventDTO] -> AppContextM (Either AppError [EventDTO])
createEvents branchUuid eventsCreateDto = do
  eitherBranch <- getBranchById branchUuid
  case eitherBranch of
    Right _ -> do
      let events = fromDTOs eventsCreateDto
      insertEventsToBranch branchUuid events
      result <- recompileKnowledgeModel branchUuid
      case result of
        Right km -> return . Right . toDTOs $ events
        Left error -> return . Left $ error
    Left error -> return . Left $ error

deleteEvents :: String -> AppContextM (Maybe AppError)
deleteEvents branchUuid = do
  eitherBranch <- getBranchById branchUuid
  case eitherBranch of
    Right _ -> do
      deleteEventsAtBranch branchUuid
      result <- recompileKnowledgeModel branchUuid
      case result of
        Right km -> return Nothing
        Left error -> return . Just $ error
    Left error -> return . Just $ error
