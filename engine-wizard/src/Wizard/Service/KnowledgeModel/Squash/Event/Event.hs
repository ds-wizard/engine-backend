module Wizard.Service.KnowledgeModel.Squash.Event.Event where

import Shared.Model.Event.Event
import Wizard.Service.KnowledgeModel.Squash.Event.Answer ()
import Wizard.Service.KnowledgeModel.Squash.Event.Chapter ()
import Wizard.Service.KnowledgeModel.Squash.Event.Choice ()
import Wizard.Service.KnowledgeModel.Squash.Event.Common
import Wizard.Service.KnowledgeModel.Squash.Event.Expert ()
import Wizard.Service.KnowledgeModel.Squash.Event.Integration ()
import Wizard.Service.KnowledgeModel.Squash.Event.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Squash.Event.Metric ()
import Wizard.Service.KnowledgeModel.Squash.Event.Phase ()
import Wizard.Service.KnowledgeModel.Squash.Event.Question ()
import Wizard.Service.KnowledgeModel.Squash.Event.Reference ()
import Wizard.Service.KnowledgeModel.Squash.Event.Tag ()

instance SimpleEventSquash Event where
  isSimpleEventSquashApplicable (EditAnswerEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditChapterEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditChoiceEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditExpertEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditIntegrationEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditKnowledgeModelEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditMetricEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditPhaseEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditQuestionEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditReferenceEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable (EditTagEvent' event) = isSimpleEventSquashApplicable event
  isSimpleEventSquashApplicable _ = False
  --  --------------------------------------
  isReorderEventSquashApplicable (EditAnswerEvent' previousEvent) (EditAnswerEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditChapterEvent' previousEvent) (EditChapterEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditChoiceEvent' previousEvent) (EditChoiceEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditExpertEvent' previousEvent) (EditExpertEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditIntegrationEvent' previousEvent) (EditIntegrationEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditKnowledgeModelEvent' previousEvent) (EditKnowledgeModelEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditMetricEvent' previousEvent) (EditMetricEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditPhaseEvent' previousEvent) (EditPhaseEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditQuestionEvent' previousEvent) (EditQuestionEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditReferenceEvent' previousEvent) (EditReferenceEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable (EditTagEvent' previousEvent) (EditTagEvent' event) =
    isReorderEventSquashApplicable previousEvent event
  isReorderEventSquashApplicable _ _ = False
  --  --------------------------------------
  isTypeChanged (EditQuestionEvent' oldEvent) (EditQuestionEvent' newEvent) = isTypeChanged oldEvent newEvent
  isTypeChanged (EditReferenceEvent' oldEvent) (EditReferenceEvent' newEvent) = isTypeChanged oldEvent newEvent
  isTypeChanged _ _ = False
  --  --------------------------------------
  simpleSquashEvent mPreviousEvent (EditAnswerEvent' oldEvent) (EditAnswerEvent' newEvent) =
    EditAnswerEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditChapterEvent' oldEvent) (EditChapterEvent' newEvent) =
    EditChapterEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditChoiceEvent' oldEvent) (EditChoiceEvent' newEvent) =
    EditChoiceEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditExpertEvent' oldEvent) (EditExpertEvent' newEvent) =
    EditExpertEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditIntegrationEvent' oldEvent) (EditIntegrationEvent' newEvent) =
    EditIntegrationEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditKnowledgeModelEvent' oldEvent) (EditKnowledgeModelEvent' newEvent) =
    EditKnowledgeModelEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditMetricEvent' oldEvent) (EditMetricEvent' newEvent) =
    EditMetricEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditPhaseEvent' oldEvent) (EditPhaseEvent' newEvent) =
    EditPhaseEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditQuestionEvent' oldEvent) (EditQuestionEvent' newEvent) =
    EditQuestionEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditReferenceEvent' oldEvent) (EditReferenceEvent' newEvent) =
    EditReferenceEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent mPreviousEvent (EditTagEvent' oldEvent) (EditTagEvent' newEvent) =
    EditTagEvent' $ simpleSquashEvent mPreviousEvent oldEvent newEvent
  simpleSquashEvent _ _ newEvent = newEvent
