module Wizard.Database.DAO.Event.EventDAO where

import Shared.Database.BSON.Event.Answer ()
import Shared.Database.BSON.Event.Chapter ()
import Shared.Database.BSON.Event.Common
import Shared.Database.BSON.Event.Expert ()
import Shared.Database.BSON.Event.KnowledgeModel ()
import Shared.Database.BSON.Event.Question ()
import Shared.Database.BSON.Event.Reference ()
import Shared.Database.DAO.Common
import Shared.Model.Event.Event
import Wizard.Database.BSON.Branch.BranchWithEvents ()
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

updateEventsInBranch :: String -> [Event] -> AppContextM ()
updateEventsInBranch branchUuid events =
  createPartialUpdateByFn' collection "uuid" branchUuid "events" (convertEventToBSON <$> events)

deleteEventsAtBranch :: String -> AppContextM ()
deleteEventsAtBranch branchUuid =
  createPartialUpdateByFn' collection "uuid" branchUuid "events" (convertEventToBSON <$> [])
