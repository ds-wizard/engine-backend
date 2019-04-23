module Database.BSON.Event.Common where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Database.BSON.Event.Answer ()
import Database.BSON.Event.Chapter ()
import Database.BSON.Event.Expert ()
import Database.BSON.Event.Integration ()
import Database.BSON.Event.KnowledgeModel ()
import Database.BSON.Event.Question ()
import Database.BSON.Event.Reference ()
import Database.BSON.Event.Tag ()
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent

chooseEventDeserializator :: BSON.Document -> Maybe Event
chooseEventDeserializator doc = do
  eventType <- BSON.lookup "eventType" doc
  case eventType of
    "AddKnowledgeModelEvent" ->
      Just . AddKnowledgeModelEvent' . fromJust $ (fromBSON doc :: Maybe AddKnowledgeModelEvent)
    "EditKnowledgeModelEvent" ->
      Just . EditKnowledgeModelEvent' . fromJust $ (fromBSON doc :: Maybe EditKnowledgeModelEvent)
    "AddChapterEvent" -> Just . AddChapterEvent' . fromJust $ (fromBSON doc :: Maybe AddChapterEvent)
    "EditChapterEvent" -> Just . EditChapterEvent' . fromJust $ (fromBSON doc :: Maybe EditChapterEvent)
    "DeleteChapterEvent" -> Just . DeleteChapterEvent' . fromJust $ (fromBSON doc :: Maybe DeleteChapterEvent)
    "AddQuestionEvent" -> Just . AddQuestionEvent' . fromJust $ (fromBSON doc :: Maybe AddQuestionEvent)
    "EditQuestionEvent" -> Just . EditQuestionEvent' . fromJust $ (fromBSON doc :: Maybe EditQuestionEvent)
    "DeleteQuestionEvent" -> Just . DeleteQuestionEvent' . fromJust $ (fromBSON doc :: Maybe DeleteQuestionEvent)
    "AddAnswerEvent" -> Just . AddAnswerEvent' . fromJust $ (fromBSON doc :: Maybe AddAnswerEvent)
    "EditAnswerEvent" -> Just . EditAnswerEvent' . fromJust $ (fromBSON doc :: Maybe EditAnswerEvent)
    "DeleteAnswerEvent" -> Just . DeleteAnswerEvent' . fromJust $ (fromBSON doc :: Maybe DeleteAnswerEvent)
    "AddExpertEvent" -> Just . AddExpertEvent' . fromJust $ (fromBSON doc :: Maybe AddExpertEvent)
    "EditExpertEvent" -> Just . EditExpertEvent' . fromJust $ (fromBSON doc :: Maybe EditExpertEvent)
    "DeleteExpertEvent" -> Just . DeleteExpertEvent' . fromJust $ (fromBSON doc :: Maybe DeleteExpertEvent)
    "AddReferenceEvent" -> Just . AddReferenceEvent' . fromJust $ (fromBSON doc :: Maybe AddReferenceEvent)
    "EditReferenceEvent" -> Just . EditReferenceEvent' . fromJust $ (fromBSON doc :: Maybe EditReferenceEvent)
    "DeleteReferenceEvent" -> Just . DeleteReferenceEvent' . fromJust $ (fromBSON doc :: Maybe DeleteReferenceEvent)
    "AddTagEvent" -> Just . AddTagEvent' . fromJust $ (fromBSON doc :: Maybe AddTagEvent)
    "EditTagEvent" -> Just . EditTagEvent' . fromJust $ (fromBSON doc :: Maybe EditTagEvent)
    "DeleteTagEvent" -> Just . DeleteTagEvent' . fromJust $ (fromBSON doc :: Maybe DeleteTagEvent)
    "AddIntegrationEvent" -> Just . AddIntegrationEvent' . fromJust $ (fromBSON doc :: Maybe AddIntegrationEvent)
    "EditIntegrationEvent" -> Just . EditIntegrationEvent' . fromJust $ (fromBSON doc :: Maybe EditIntegrationEvent)
    "DeleteIntegrationEvent" ->
      Just . DeleteIntegrationEvent' . fromJust $ (fromBSON doc :: Maybe DeleteIntegrationEvent)

convertEventToBSON :: Event -> BSON.Document
convertEventToBSON (AddKnowledgeModelEvent' event) = toBSON event
convertEventToBSON (EditKnowledgeModelEvent' event) = toBSON event
convertEventToBSON (AddChapterEvent' event) = toBSON event
convertEventToBSON (EditChapterEvent' event) = toBSON event
convertEventToBSON (DeleteChapterEvent' event) = toBSON event
convertEventToBSON (AddQuestionEvent' event) = toBSON event
convertEventToBSON (EditQuestionEvent' event) = toBSON event
convertEventToBSON (DeleteQuestionEvent' event) = toBSON event
convertEventToBSON (AddAnswerEvent' event) = toBSON event
convertEventToBSON (EditAnswerEvent' event) = toBSON event
convertEventToBSON (DeleteAnswerEvent' event) = toBSON event
convertEventToBSON (AddExpertEvent' event) = toBSON event
convertEventToBSON (EditExpertEvent' event) = toBSON event
convertEventToBSON (DeleteExpertEvent' event) = toBSON event
convertEventToBSON (AddReferenceEvent' event) = toBSON event
convertEventToBSON (EditReferenceEvent' event) = toBSON event
convertEventToBSON (DeleteReferenceEvent' event) = toBSON event
convertEventToBSON (AddTagEvent' event) = toBSON event
convertEventToBSON (EditTagEvent' event) = toBSON event
convertEventToBSON (DeleteTagEvent' event) = toBSON event
convertEventToBSON (AddIntegrationEvent' event) = toBSON event
convertEventToBSON (EditIntegrationEvent' event) = toBSON event
convertEventToBSON (DeleteIntegrationEvent' event) = toBSON event
