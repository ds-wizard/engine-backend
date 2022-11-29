module Shared.Api.Resource.Event.EventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Event.AnswerEventJM ()
import Shared.Api.Resource.Event.ChapterEventJM ()
import Shared.Api.Resource.Event.ChoiceEventJM ()
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.ExpertEventJM ()
import Shared.Api.Resource.Event.IntegrationEventJM ()
import Shared.Api.Resource.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.Event.MetricEventJM ()
import Shared.Api.Resource.Event.MoveEventJM ()
import Shared.Api.Resource.Event.PhaseEventJM ()
import Shared.Api.Resource.Event.QuestionEventJM ()
import Shared.Api.Resource.Event.ReferenceEventJM ()
import Shared.Api.Resource.Event.TagEventJM ()
import Shared.Model.Event.Event
import Shared.Util.Aeson

instance ToJSON Event where
  toJSON = toSumJSONWithTypeField "eventType" ""

instance FromJSON Event where
  parseJSON (Object o) = do
    eventType <- o .: "eventType"
    case eventType of
      "AddKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (AddKnowledgeModelEvent' event)
      "EditKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (EditKnowledgeModelEvent' event)
      "AddChapterEvent" -> parseJSON (Object o) >>= \event -> return (AddChapterEvent' event)
      "EditChapterEvent" -> parseJSON (Object o) >>= \event -> return (EditChapterEvent' event)
      "DeleteChapterEvent" -> parseJSON (Object o) >>= \event -> return (DeleteChapterEvent' event)
      "AddQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddQuestionEvent' event)
      "EditQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditQuestionEvent' event)
      "DeleteQuestionEvent" -> parseJSON (Object o) >>= \event -> return (DeleteQuestionEvent' event)
      "AddAnswerEvent" -> parseJSON (Object o) >>= \event -> return (AddAnswerEvent' event)
      "EditAnswerEvent" -> parseJSON (Object o) >>= \event -> return (EditAnswerEvent' event)
      "DeleteAnswerEvent" -> parseJSON (Object o) >>= \event -> return (DeleteAnswerEvent' event)
      "AddChoiceEvent" -> parseJSON (Object o) >>= \event -> return (AddChoiceEvent' event)
      "EditChoiceEvent" -> parseJSON (Object o) >>= \event -> return (EditChoiceEvent' event)
      "DeleteChoiceEvent" -> parseJSON (Object o) >>= \event -> return (DeleteChoiceEvent' event)
      "AddExpertEvent" -> parseJSON (Object o) >>= \event -> return (AddExpertEvent' event)
      "EditExpertEvent" -> parseJSON (Object o) >>= \event -> return (EditExpertEvent' event)
      "DeleteExpertEvent" -> parseJSON (Object o) >>= \event -> return (DeleteExpertEvent' event)
      "AddReferenceEvent" -> parseJSON (Object o) >>= \event -> return (AddReferenceEvent' event)
      "EditReferenceEvent" -> parseJSON (Object o) >>= \event -> return (EditReferenceEvent' event)
      "DeleteReferenceEvent" -> parseJSON (Object o) >>= \event -> return (DeleteReferenceEvent' event)
      "AddTagEvent" -> parseJSON (Object o) >>= \event -> return (AddTagEvent' event)
      "EditTagEvent" -> parseJSON (Object o) >>= \event -> return (EditTagEvent' event)
      "DeleteTagEvent" -> parseJSON (Object o) >>= \event -> return (DeleteTagEvent' event)
      "AddIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (AddIntegrationEvent' event)
      "EditIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (EditIntegrationEvent' event)
      "DeleteIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (DeleteIntegrationEvent' event)
      "AddMetricEvent" -> parseJSON (Object o) >>= \event -> return (AddMetricEvent' event)
      "EditMetricEvent" -> parseJSON (Object o) >>= \event -> return (EditMetricEvent' event)
      "DeleteMetricEvent" -> parseJSON (Object o) >>= \event -> return (DeleteMetricEvent' event)
      "AddPhaseEvent" -> parseJSON (Object o) >>= \event -> return (AddPhaseEvent' event)
      "EditPhaseEvent" -> parseJSON (Object o) >>= \event -> return (EditPhaseEvent' event)
      "DeletePhaseEvent" -> parseJSON (Object o) >>= \event -> return (DeletePhaseEvent' event)
      "MoveQuestionEvent" -> parseJSON (Object o) >>= \event -> return (MoveQuestionEvent' event)
      "MoveAnswerEvent" -> parseJSON (Object o) >>= \event -> return (MoveAnswerEvent' event)
      "MoveChoiceEvent" -> parseJSON (Object o) >>= \event -> return (MoveChoiceEvent' event)
      "MoveExpertEvent" -> parseJSON (Object o) >>= \event -> return (MoveExpertEvent' event)
      "MoveReferenceEvent" -> parseJSON (Object o) >>= \event -> return (MoveReferenceEvent' event)
      _ -> fail "One of the events has unsupported eventType"
  parseJSON _ = mzero
