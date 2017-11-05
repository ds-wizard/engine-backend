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

-- import Common.Types
import Context
import Database.BSON.Event.Answer
import Database.BSON.Event.Chapter
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

-- import Model.Event.Event
findKmcWithEventsById :: Context
                      -> String
                      -> IO (Maybe KnowledgeModelContainerWithEvents)
findKmcWithEventsById context kmcUuid = do
  let action = findOne $ select ["uuid" =: kmcUuid] kmcCollection
  maybeKmcWithEvents <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybeKmcWithEvents of
    Just kmcWithEvents -> return . fromBSON $ kmcWithEvents
    Nothing -> return Nothing

--  let action = rest =<< findOne (select ["uuid" =: kmcUuid] kmcCollection)
--  maybeKmcWithEvents <- runMongoDBPoolDef action (context ^. ctxDbPool)
--  case maybeKmcWithEvents of
--    Just kmcweWithEvents -> return . Just . fromBSON $ kmcweWithEvents
--    _ -> return Nothing
--  case maybeKmcWithEvents of
--    Just kmcWithEvents -> do
--      let events = kmcWithEvents ^. kmcweWithEvents
--      return $ fmap (fromJust . chooseEventDeserializator) events
--    _ -> return Nothing
insertEventsToKmc :: Context -> String -> [Event] -> IO ()
insertEventsToKmc context kmcUuid events = do
  let action =
        modify
          (select ["uuid" =: kmcUuid] kmcCollection)
          [ "$push" =:
            ["events" =: ["$each" =: (convertEventToBSON <$> events)]]
          ]
  runMongoDBPoolDef action (context ^. ctxDbPool)

--  let action = insertMany kmc (convertEventToBSON <$> events)
--  runMongoDBPoolDef action (context ^. ctxDbPool)
-- deleteEvents :: Context -> IO ()
-- deleteEvents context = do
--     let action = delete $ select [] eventCollection
--     runMongoDBPoolDef action (context ^. ctxDbPool)
deleteEventAtKmc :: Context -> String -> IO ()
deleteEventAtKmc context kmcUuid = do
  let emptyEvents = convertEventToBSON <$> []
  let action =
        modify
          (select ["uuid" =: kmcUuid] kmcCollection)
          ["$set" =: ["events" =: emptyEvents]]
  runMongoDBPoolDef action (context ^. ctxDbPool)
--  let action = deleteOne $ select ["uuid" =: userUuid] eventCollection
--  runMongoDBPoolDef action (context ^. ctxDbPool)
