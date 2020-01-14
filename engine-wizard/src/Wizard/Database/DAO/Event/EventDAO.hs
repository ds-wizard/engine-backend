module Wizard.Database.DAO.Event.EventDAO where

import Shared.Model.Event.Event
import Wizard.Database.BSON.Branch.BranchWithEvents ()
import Wizard.Database.BSON.Event.Answer ()
import Wizard.Database.BSON.Event.Chapter ()
import Wizard.Database.BSON.Event.Common
import Wizard.Database.BSON.Event.Expert ()
import Wizard.Database.BSON.Event.KnowledgeModel ()
import Wizard.Database.BSON.Event.Question ()
import Wizard.Database.BSON.Event.Reference ()
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext

updateEventsInBranch :: String -> [Event] -> AppContextM ()
updateEventsInBranch branchUuid events = do
  createPartialUpdateByFn' collection "uuid" branchUuid "events" (convertEventToBSON <$> events)

deleteEventsAtBranch :: String -> AppContextM ()
deleteEventsAtBranch branchUuid = do
  createPartialUpdateByFn' collection "uuid" branchUuid "events" (convertEventToBSON <$> [])
