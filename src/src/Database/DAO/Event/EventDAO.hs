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

import Common.Error
import Common.Context
import Database.BSON.Event.Answer
import Database.BSON.Event.Chapter
import Database.BSON.Event.Common
import Database.BSON.Event.Expert
import Database.BSON.Event.FollowUpQuestion
import Database.BSON.Event.KnowledgeModel
import Database.BSON.Event.Question
import Database.BSON.Event.Reference
import Database.BSON.KnowledgeModelContainer.KnowledgeModelContainerWithEvents
import Database.DAO.Common
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Model.Event.Common
import Model.Event.Event
import Model.KnowledgeModelContainer.KnowledgeModelContainer

findKmcWithEventsById :: Context
                      -> String
                      -> IO (Either AppError KnowledgeModelContainerWithEvents)
findKmcWithEventsById context kmcUuid = do
  let action = findOne $ select ["uuid" =: kmcUuid] kmcCollection
  maybeKmcWithEventsS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeKmcWithEventsS

insertEventsToKmc :: Context -> String -> [Event] -> IO ()
insertEventsToKmc context kmcUuid events = do
  let action =
        modify
          (select ["uuid" =: kmcUuid] kmcCollection)
          [ "$push" =:
            ["events" =: ["$each" =: (convertEventToBSON <$> events)]]
          ]
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteEventAtKmc :: Context -> String -> IO ()
deleteEventAtKmc context kmcUuid = do
  let emptyEvents = convertEventToBSON <$> []
  let action =
        modify
          (select ["uuid" =: kmcUuid] kmcCollection)
          ["$set" =: ["events" =: emptyEvents]]
  runMongoDBPoolDef action (context ^. ctxDbPool)
