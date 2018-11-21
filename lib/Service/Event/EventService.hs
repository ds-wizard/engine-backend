module Service.Event.EventService where

import Control.Lens ((^.))

import Api.Resource.Event.EventDTO
import Database.DAO.Event.EventDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Service.Branch.BranchService
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelService

getEvents :: String -> AppContextM (Either AppError [EventDTO])
getEvents branchUuid =
  heFindBranchWithEventsById branchUuid $ \branchWithEvents -> return . Right . toDTOs $ branchWithEvents ^. events

createEvents :: String -> [EventDTO] -> AppContextM (Either AppError [EventDTO])
createEvents branchUuid eventsCreateDto =
  heGetBranchById branchUuid $ \_ -> do
    let events = fromDTOs eventsCreateDto
    insertEventsToBranch branchUuid events
    result <- recompileKnowledgeModel branchUuid
    heRecompileKnowledgeModel branchUuid $ \_ -> return . Right . toDTOs $ events

deleteEvents :: String -> AppContextM (Maybe AppError)
deleteEvents branchUuid =
  hmGetBranchById branchUuid $ \_ -> do
    deleteEventsAtBranch branchUuid
    hmRecompileKnowledgeModel branchUuid $ \_ -> return Nothing
