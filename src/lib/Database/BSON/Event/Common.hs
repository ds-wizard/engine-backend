module Database.BSON.Event.Common where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Database.BSON.Event.Answer
import Database.BSON.Event.Chapter
import Database.BSON.Event.Expert
import Database.BSON.Event.FollowUpQuestion
import Database.BSON.Event.KnowledgeModel
import Database.BSON.Event.Question
import Database.BSON.Event.Reference
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

chooseEventDeserializator :: BSON.Document -> Maybe Event
chooseEventDeserializator doc = do
  eventType <- BSON.lookup "eventType" doc
  case eventType of
    "AddKnowledgeModelEvent" ->
      Just . AddKnowledgeModelEvent' . fromJust $
      (fromBSON doc :: Maybe AddKnowledgeModelEvent)
    "EditKnowledgeModelEvent" ->
      Just . EditKnowledgeModelEvent' . fromJust $
      (fromBSON doc :: Maybe EditKnowledgeModelEvent)
    "AddChapterEvent" ->
      Just . AddChapterEvent' . fromJust $
      (fromBSON doc :: Maybe AddChapterEvent)
    "EditChapterEvent" ->
      Just . EditChapterEvent' . fromJust $
      (fromBSON doc :: Maybe EditChapterEvent)
    "DeleteChapterEvent" ->
      Just . DeleteChapterEvent' . fromJust $
      (fromBSON doc :: Maybe DeleteChapterEvent)
    "AddQuestionEvent" ->
      Just . AddQuestionEvent' . fromJust $
      (fromBSON doc :: Maybe AddQuestionEvent)
    "EditQuestionEvent" ->
      Just . EditQuestionEvent' . fromJust $
      (fromBSON doc :: Maybe EditQuestionEvent)
    "DeleteQuestionEvent" ->
      Just . DeleteQuestionEvent' . fromJust $
      (fromBSON doc :: Maybe DeleteQuestionEvent)
    "AddAnswerEvent" ->
      Just . AddAnswerEvent' . fromJust $ (fromBSON doc :: Maybe AddAnswerEvent)
    "EditAnswerEvent" ->
      Just . EditAnswerEvent' . fromJust $
      (fromBSON doc :: Maybe EditAnswerEvent)
    "DeleteAnswerEvent" ->
      Just . DeleteAnswerEvent' . fromJust $
      (fromBSON doc :: Maybe DeleteAnswerEvent)
    "AddExpertEvent" ->
      Just . AddExpertEvent' . fromJust $ (fromBSON doc :: Maybe AddExpertEvent)
    "EditExpertEvent" ->
      Just . EditExpertEvent' . fromJust $
      (fromBSON doc :: Maybe EditExpertEvent)
    "DeleteExpertEvent" ->
      Just . DeleteExpertEvent' . fromJust $
      (fromBSON doc :: Maybe DeleteExpertEvent)
    "AddReferenceEvent" ->
      Just . AddReferenceEvent' . fromJust $
      (fromBSON doc :: Maybe AddReferenceEvent)
    "EditReferenceEvent" ->
      Just . EditReferenceEvent' . fromJust $
      (fromBSON doc :: Maybe EditReferenceEvent)
    "DeleteReferenceEvent" ->
      Just . DeleteReferenceEvent' . fromJust $
      (fromBSON doc :: Maybe DeleteReferenceEvent)
    "AddFollowUpQuestionEvent" ->
      Just . AddFollowUpQuestionEvent' . fromJust $
      (fromBSON doc :: Maybe AddFollowUpQuestionEvent)
    "EditFollowUpQuestionEvent" ->
      Just . EditFollowUpQuestionEvent' . fromJust $
      (fromBSON doc :: Maybe EditFollowUpQuestionEvent)
    "DeleteFollowUpQuestionEvent" ->
      Just . DeleteFollowUpQuestionEvent' . fromJust $
      (fromBSON doc :: Maybe DeleteFollowUpQuestionEvent)

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
convertEventToBSON (AddFollowUpQuestionEvent' event) = toBSON event
convertEventToBSON (EditFollowUpQuestionEvent' event) = toBSON event
convertEventToBSON (DeleteFollowUpQuestionEvent' event) = toBSON event
