module Wizard.Api.Resource.Event.EventJM where

import Control.Monad
import Data.Aeson

import Wizard.Api.Resource.Event.AnswerEventJM ()
import Wizard.Api.Resource.Event.ChapterEventJM ()
import Wizard.Api.Resource.Event.EventDTO
import Wizard.Api.Resource.Event.EventFieldJM ()
import Wizard.Api.Resource.Event.ExpertEventJM ()
import Wizard.Api.Resource.Event.IntegrationEventJM ()
import Wizard.Api.Resource.Event.KnowledgeModelEventJM ()
import Wizard.Api.Resource.Event.MoveEventJM ()
import Wizard.Api.Resource.Event.QuestionEventJM ()
import Wizard.Api.Resource.Event.ReferenceEventJM ()
import Wizard.Api.Resource.Event.TagEventJM ()

instance ToJSON EventDTO where
  toJSON (AddKnowledgeModelEventDTO' event) = toJSON event
  toJSON (EditKnowledgeModelEventDTO' event) = toJSON event
  toJSON (AddChapterEventDTO' event) = toJSON event
  toJSON (EditChapterEventDTO' event) = toJSON event
  toJSON (DeleteChapterEventDTO' event) = toJSON event
  toJSON (AddQuestionEventDTO' event) = toJSON event
  toJSON (EditQuestionEventDTO' event) = toJSON event
  toJSON (DeleteQuestionEventDTO' event) = toJSON event
  toJSON (AddAnswerEventDTO' event) = toJSON event
  toJSON (EditAnswerEventDTO' event) = toJSON event
  toJSON (DeleteAnswerEventDTO' event) = toJSON event
  toJSON (AddExpertEventDTO' event) = toJSON event
  toJSON (EditExpertEventDTO' event) = toJSON event
  toJSON (DeleteExpertEventDTO' event) = toJSON event
  toJSON (AddReferenceEventDTO' event) = toJSON event
  toJSON (EditReferenceEventDTO' event) = toJSON event
  toJSON (DeleteReferenceEventDTO' event) = toJSON event
  toJSON (AddTagEventDTO' event) = toJSON event
  toJSON (EditTagEventDTO' event) = toJSON event
  toJSON (DeleteTagEventDTO' event) = toJSON event
  toJSON (AddIntegrationEventDTO' event) = toJSON event
  toJSON (EditIntegrationEventDTO' event) = toJSON event
  toJSON (DeleteIntegrationEventDTO' event) = toJSON event
  toJSON (MoveQuestionEventDTO' event) = toJSON event
  toJSON (MoveAnswerEventDTO' event) = toJSON event
  toJSON (MoveExpertEventDTO' event) = toJSON event
  toJSON (MoveReferenceEventDTO' event) = toJSON event

instance FromJSON EventDTO where
  parseJSON (Object o) = do
    eventType <- o .: "eventType"
    case eventType of
      "AddKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (AddKnowledgeModelEventDTO' event)
      "EditKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (EditKnowledgeModelEventDTO' event)
      "AddChapterEvent" -> parseJSON (Object o) >>= \event -> return (AddChapterEventDTO' event)
      "EditChapterEvent" -> parseJSON (Object o) >>= \event -> return (EditChapterEventDTO' event)
      "DeleteChapterEvent" -> parseJSON (Object o) >>= \event -> return (DeleteChapterEventDTO' event)
      "AddQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddQuestionEventDTO' event)
      "EditQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditQuestionEventDTO' event)
      "DeleteQuestionEvent" -> parseJSON (Object o) >>= \event -> return (DeleteQuestionEventDTO' event)
      "AddAnswerEvent" -> parseJSON (Object o) >>= \event -> return (AddAnswerEventDTO' event)
      "EditAnswerEvent" -> parseJSON (Object o) >>= \event -> return (EditAnswerEventDTO' event)
      "DeleteAnswerEvent" -> parseJSON (Object o) >>= \event -> return (DeleteAnswerEventDTO' event)
      "AddExpertEvent" -> parseJSON (Object o) >>= \event -> return (AddExpertEventDTO' event)
      "EditExpertEvent" -> parseJSON (Object o) >>= \event -> return (EditExpertEventDTO' event)
      "DeleteExpertEvent" -> parseJSON (Object o) >>= \event -> return (DeleteExpertEventDTO' event)
      "AddReferenceEvent" -> parseJSON (Object o) >>= \event -> return (AddReferenceEventDTO' event)
      "EditReferenceEvent" -> parseJSON (Object o) >>= \event -> return (EditReferenceEventDTO' event)
      "DeleteReferenceEvent" -> parseJSON (Object o) >>= \event -> return (DeleteReferenceEventDTO' event)
      "AddTagEvent" -> parseJSON (Object o) >>= \event -> return (AddTagEventDTO' event)
      "EditTagEvent" -> parseJSON (Object o) >>= \event -> return (EditTagEventDTO' event)
      "DeleteTagEvent" -> parseJSON (Object o) >>= \event -> return (DeleteTagEventDTO' event)
      "AddIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (AddIntegrationEventDTO' event)
      "EditIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (EditIntegrationEventDTO' event)
      "DeleteIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (DeleteIntegrationEventDTO' event)
      "MoveQuestionEvent" -> parseJSON (Object o) >>= \event -> return (MoveQuestionEventDTO' event)
      "MoveAnswerEvent" -> parseJSON (Object o) >>= \event -> return (MoveAnswerEventDTO' event)
      "MoveExpertEvent" -> parseJSON (Object o) >>= \event -> return (MoveExpertEventDTO' event)
      "MoveReferenceEvent" -> parseJSON (Object o) >>= \event -> return (MoveReferenceEventDTO' event)
      _ -> fail "One of the events has unsupported eventType"
  parseJSON _ = mzero
