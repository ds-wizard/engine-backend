module Wizard.Service.Event.EventService where

import Shared.Model.Event.Event
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchService

updateEvents :: String -> [Event] -> AppContextM [Event]
updateEvents branchUuid eventsCreateDto =
  runInTransaction $ do
    _ <- getBranchById branchUuid
    updateEventsInBranch branchUuid eventsCreateDto
    return eventsCreateDto

deleteEvents :: String -> AppContextM ()
deleteEvents branchUuid =
  runInTransaction $ do
    _ <- getBranchById branchUuid
    deleteEventsAtBranch branchUuid
    return ()
