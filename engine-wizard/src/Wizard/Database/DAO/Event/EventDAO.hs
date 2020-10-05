module Wizard.Database.DAO.Event.EventDAO where

import Shared.Database.BSON.Event.Common ()
import Shared.Database.DAO.Common
import Shared.Model.Event.Event
import Wizard.Database.BSON.Branch.BranchWithEvents ()
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

updateEventsInBranch :: String -> [Event] -> AppContextM ()
updateEventsInBranch branchUuid = createPartialUpdateByFn' collection "uuid" branchUuid "events"

deleteEventsAtBranch :: String -> AppContextM ()
deleteEventsAtBranch branchUuid = createPartialUpdateByFn' collection "uuid" branchUuid "events" ([] :: [Event])
