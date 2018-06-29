module Database.DAO.Event.EventDAO where

import Database.MongoDB ((=:), findOne, modify, select)

import Common.Error
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
import Model.Branch.Branch
import Model.Context.AppContext
import Model.Event.Event

findBranchWithEventsById :: String -> AppContextM (Either AppError BranchWithEvents)
findBranchWithEventsById branchUuid = do
  let action = findOne $ select ["uuid" =: branchUuid] branchCollection
  maybeBranchWithEventsS <- runDB action
  return . deserializeMaybeEntity $ maybeBranchWithEventsS

insertEventsToBranch :: String -> [Event] -> AppContextM ()
insertEventsToBranch branchUuid events = do
  let action =
        modify
          (select ["uuid" =: branchUuid] branchCollection)
          ["$push" =: ["events" =: ["$each" =: (convertEventToBSON <$> events)]]]
  runDB action

deleteEventsAtBranch :: String -> AppContextM ()
deleteEventsAtBranch branchUuid = do
  let emptyEvents = convertEventToBSON <$> []
  let action = modify (select ["uuid" =: branchUuid] branchCollection) ["$set" =: ["events" =: emptyEvents]]
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetBranch branchUuid callback = do
  eitherBranch <- findBranchWithEventsById branchUuid
  case eitherBranch of
    Right branch -> callback branch
    Left error -> return . Left $ error
