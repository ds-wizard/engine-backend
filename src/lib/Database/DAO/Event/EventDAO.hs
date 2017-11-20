module Database.DAO.Event.EventDAO where

import Control.Lens ((^.))
import Data.Bson
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insertMany, fetch, modify, save, merge,
        delete, deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Context
import Common.Error
import Database.BSON.Branch.BranchWithEvents
import Database.BSON.Event.Answer
import Database.BSON.Event.Chapter
import Database.BSON.Event.Common
import Database.BSON.Event.Expert
import Database.BSON.Event.FollowUpQuestion
import Database.BSON.Event.KnowledgeModel
import Database.BSON.Event.Question
import Database.BSON.Event.Reference
import Database.DAO.Branch.BranchDAO
import Database.DAO.Common
import Model.Branch.Branch
import Model.Event.Common
import Model.Event.Event

findBranchWithEventsById :: Context -> String -> IO (Either AppError BranchWithEvents)
findBranchWithEventsById context branchUuid = do
  let action = findOne $ select ["uuid" =: branchUuid] branchCollection
  maybeBranchWithEventsS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeBranchWithEventsS

insertEventsToBranch :: Context -> String -> [Event] -> IO ()
insertEventsToBranch context branchUuid events = do
  let action =
        modify
          (select ["uuid" =: branchUuid] branchCollection)
          ["$push" =: ["events" =: ["$each" =: (convertEventToBSON <$> events)]]]
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteEventAtBranch :: Context -> String -> IO ()
deleteEventAtBranch context branchUuid = do
  let emptyEvents = convertEventToBSON <$> []
  let action = modify (select ["uuid" =: branchUuid] branchCollection) ["$set" =: ["events" =: emptyEvents]]
  runMongoDBPoolDef action (context ^. ctxDbPool)
