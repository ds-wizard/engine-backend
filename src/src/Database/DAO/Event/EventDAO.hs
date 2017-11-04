module Database.DAO.Event.EventDAO where

import Control.Lens ((^.))
import Data.Bson
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insertMany, fetch, save, merge, delete,
        deleteOne, (=:), rest)
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
import Database.DAO.Common
import KMMigration.Migration.Event.Common
import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.DeleteChapterEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Event
import Model.Event.Expert.AddExpertEvent
import Model.Event.Expert.DeleteExpertEvent
import Model.Event.Expert.EditExpertEvent
import Model.Event.FollowUpQuestion.AddFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.DeleteFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import Model.Event.KnowledgeModel.AddKnowledgeModelEvent
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.Event.Question.AddQuestionEvent
import Model.Event.Question.DeleteQuestionEvent
import Model.Event.Question.EditQuestionEvent
import Model.Event.Reference.AddReferenceEvent
import Model.Event.Reference.DeleteReferenceEvent
import Model.Event.Reference.EditReferenceEvent

-- import Model.Event.Event
eventCollection = "events"

findEvents :: Context -> IO [Event]
findEvents context = do
  let action = rest =<< find (select [] eventCollection)
  events <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return $ fmap (fromJust . chooseEventDeserializator) events

chooseEventDeserializator :: Document -> Maybe Event
chooseEventDeserializator doc = do
  eventType <- BSON.lookup "eventType" doc
  case eventType of
    "AddKnowledgeModelEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe AddKnowledgeModelEvent)
    "EditKnowledgeModelEvent" ->
      Just . MkEvent . fromJust $
      (fromBSON doc :: Maybe EditKnowledgeModelEvent)
    "AddChapterEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe AddChapterEvent)
    "EditChapterEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe EditChapterEvent)
    "DeleteChapterEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe DeleteChapterEvent)
    "AddQuestionEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe AddQuestionEvent)
    "EditQuestionEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe EditQuestionEvent)
    "DeleteQuestionEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe DeleteQuestionEvent)
    "AddAnswerEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe AddAnswerEvent)
    "EditAnswerEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe EditAnswerEvent)
    "DeleteAnswerEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe DeleteAnswerEvent)
    "AddExpertEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe AddExpertEvent)
    "EditExpertEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe EditExpertEvent)
    "DeleteExpertEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe DeleteExpertEvent)
    "AddReferenceEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe AddReferenceEvent)
    "EditReferenceEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe EditReferenceEvent)
    "DeleteReferenceEvent" ->
      Just . MkEvent . fromJust $ (fromBSON doc :: Maybe DeleteReferenceEvent)
    "AddFollowUpQuestionEvent" ->
      Just . MkEvent . fromJust $
      (fromBSON doc :: Maybe AddFollowUpQuestionEvent)
    "EditFollowUpQuestionEvent" ->
      Just . MkEvent . fromJust $
      (fromBSON doc :: Maybe EditFollowUpQuestionEvent)
    "DeleteFollowUpQuestionEvent" ->
      Just . MkEvent . fromJust $
      (fromBSON doc :: Maybe DeleteFollowUpQuestionEvent)

findEventById :: Context -> String -> IO (Maybe Event)
findEventById context eventUuid = do
  let action = findOne $ select ["uuid" =: eventUuid] eventCollection
  maybeEvent <- runMongoDBPoolDef action (context ^. ctxDbPool)
  case maybeEvent of
    Just event -> return . chooseEventDeserializator $ event
    Nothing -> return Nothing

convertEventToBSON :: Event -> Document
convertEventToBSON (MkEvent event) = toBSON event

insertEvents :: Context -> [Event] -> IO [Value]
insertEvents context events = do
  let action = insertMany eventCollection (convertEventToBSON <$> events)
  runMongoDBPoolDef action (context ^. ctxDbPool)

-- deleteEvents :: Context -> IO ()
-- deleteEvents context = do
--     let action = delete $ select [] eventCollection
--     runMongoDBPoolDef action (context ^. ctxDbPool)
deleteEventById :: Context -> String -> IO ()
deleteEventById context userUuid = do
  let action = deleteOne $ select ["uuid" =: userUuid] eventCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
