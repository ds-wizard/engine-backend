module Wizard.Service.Event.EventService where

import Shared.Api.Resource.Event.EventDTO
import Shared.Service.Event.EventMapper
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchService

updateEvents :: String -> [EventDTO] -> AppContextM [EventDTO]
updateEvents branchUuid eventsCreateDto = do
  _ <- getBranchById branchUuid
  let events = fromDTOs eventsCreateDto
  updateEventsInBranch branchUuid events
  return . toDTOs $ events

deleteEvents :: String -> AppContextM ()
deleteEvents branchUuid = do
  _ <- getBranchById branchUuid
  deleteEventsAtBranch branchUuid
  return ()
