module Service.Event.EventService where

import Api.Resource.Event.EventDTO
import Database.DAO.Event.EventDAO
import Model.Context.AppContext
import Model.Error.Error
import Service.Branch.BranchService
import Service.Event.EventMapper

updateEvents :: String -> [EventDTO] -> AppContextM (Either AppError [EventDTO])
updateEvents branchUuid eventsCreateDto =
  heGetBranchById branchUuid $ \_ -> do
    let events = fromDTOs eventsCreateDto
    updateEventsInBranch branchUuid events
    return . Right . toDTOs $ events

deleteEvents :: String -> AppContextM (Maybe AppError)
deleteEvents branchUuid =
  hmGetBranchById branchUuid $ \_ -> do
    deleteEventsAtBranch branchUuid
    return Nothing
