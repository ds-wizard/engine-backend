module Shared.Database.BSON.Event.Common where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Shared.Database.BSON.Event.Answer ()
import Shared.Database.BSON.Event.Chapter ()
import Shared.Database.BSON.Event.Choice ()
import Shared.Database.BSON.Event.Expert ()
import Shared.Database.BSON.Event.Integration ()
import Shared.Database.BSON.Event.KnowledgeModel ()
import Shared.Database.BSON.Event.Move ()
import Shared.Database.BSON.Event.Question ()
import Shared.Database.BSON.Event.Reference ()
import Shared.Database.BSON.Event.Tag ()
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.Event
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.Event.Tag.TagEvent

instance FromBSON Event where
  fromBSON doc = do
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
      "AddChoiceEvent" -> Just . AddChoiceEvent' . fromJust $ (fromBSON doc :: Maybe AddChoiceEvent)
      "EditChoiceEvent" -> Just . EditChoiceEvent' . fromJust $ (fromBSON doc :: Maybe EditChoiceEvent)
      "DeleteChoiceEvent" -> Just . DeleteChoiceEvent' . fromJust $ (fromBSON doc :: Maybe DeleteChoiceEvent)
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
      "MoveQuestionEvent" -> Just . MoveQuestionEvent' . fromJust $ (fromBSON doc :: Maybe MoveQuestionEvent)
      "MoveAnswerEvent" -> Just . MoveAnswerEvent' . fromJust $ (fromBSON doc :: Maybe MoveAnswerEvent)
      "MoveChoiceEvent" -> Just . MoveChoiceEvent' . fromJust $ (fromBSON doc :: Maybe MoveChoiceEvent)
      "MoveExpertEvent" -> Just . MoveExpertEvent' . fromJust $ (fromBSON doc :: Maybe MoveExpertEvent)
      "MoveReferenceEvent" -> Just . MoveReferenceEvent' . fromJust $ (fromBSON doc :: Maybe MoveReferenceEvent)

instance ToBSON Event where
  toBSON (AddKnowledgeModelEvent' event) = toBSON event
  toBSON (EditKnowledgeModelEvent' event) = toBSON event
  toBSON (AddChapterEvent' event) = toBSON event
  toBSON (EditChapterEvent' event) = toBSON event
  toBSON (DeleteChapterEvent' event) = toBSON event
  toBSON (AddQuestionEvent' event) = toBSON event
  toBSON (EditQuestionEvent' event) = toBSON event
  toBSON (DeleteQuestionEvent' event) = toBSON event
  toBSON (AddAnswerEvent' event) = toBSON event
  toBSON (EditAnswerEvent' event) = toBSON event
  toBSON (DeleteAnswerEvent' event) = toBSON event
  toBSON (AddChoiceEvent' event) = toBSON event
  toBSON (EditChoiceEvent' event) = toBSON event
  toBSON (DeleteChoiceEvent' event) = toBSON event
  toBSON (AddExpertEvent' event) = toBSON event
  toBSON (EditExpertEvent' event) = toBSON event
  toBSON (DeleteExpertEvent' event) = toBSON event
  toBSON (AddReferenceEvent' event) = toBSON event
  toBSON (EditReferenceEvent' event) = toBSON event
  toBSON (DeleteReferenceEvent' event) = toBSON event
  toBSON (AddTagEvent' event) = toBSON event
  toBSON (EditTagEvent' event) = toBSON event
  toBSON (DeleteTagEvent' event) = toBSON event
  toBSON (AddIntegrationEvent' event) = toBSON event
  toBSON (EditIntegrationEvent' event) = toBSON event
  toBSON (DeleteIntegrationEvent' event) = toBSON event
  toBSON (MoveQuestionEvent' event) = toBSON event
  toBSON (MoveAnswerEvent' event) = toBSON event
  toBSON (MoveChoiceEvent' event) = toBSON event
  toBSON (MoveExpertEvent' event) = toBSON event
  toBSON (MoveReferenceEvent' event) = toBSON event
