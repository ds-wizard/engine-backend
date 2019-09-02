module Database.DAO.Event.EventDAO where

import Database.BSON.Branch.BranchWithEvents ()
import Database.BSON.Event.Answer ()
import Database.BSON.Event.Chapter ()
import Database.BSON.Event.Common
import Database.BSON.Event.Expert ()
import Database.BSON.Event.KnowledgeModel ()
import Database.BSON.Event.Question ()
import Database.BSON.Event.Reference ()
import Database.DAO.Branch.BranchDAO
import Database.DAO.Common
import Model.Context.AppContext
import Model.Event.Event

updateEventsInBranch :: String -> [Event] -> AppContextM ()
updateEventsInBranch branchUuid events = do
  createPartialUpdateByFn' collection "uuid" branchUuid "events" (convertEventToBSON <$> events)

deleteEventsAtBranch :: String -> AppContextM ()
deleteEventsAtBranch branchUuid = do
  createPartialUpdateByFn' collection "uuid" branchUuid "events" (convertEventToBSON <$> [])
