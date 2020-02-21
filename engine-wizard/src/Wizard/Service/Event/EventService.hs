module Wizard.Service.Event.EventService where

import Wizard.Api.Resource.Event.EventDTO
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchService
import Wizard.Service.Event.EventMapper

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
