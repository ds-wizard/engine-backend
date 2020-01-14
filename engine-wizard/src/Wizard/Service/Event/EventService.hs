module Wizard.Service.Event.EventService where

import Shared.Model.Error.Error
import Wizard.Api.Resource.Event.EventDTO
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchService
import Wizard.Service.Event.EventMapper

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
