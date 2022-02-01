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
  isTypeChanged (EditQuestionEvent' oldEvent) (EditQuestionEvent' newEvent) = isTypeChanged oldEvent newEvent
  isTypeChanged (EditReferenceEvent' oldEvent) (EditReferenceEvent' newEvent) = isTypeChanged oldEvent newEvent
  isTypeChanged _ _ = False
  --  --------------------------------------
  simpleSquashEvent (EditAnswerEvent' oldEvent) (EditAnswerEvent' newEvent) =
    EditAnswerEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditChapterEvent' oldEvent) (EditChapterEvent' newEvent) =
    EditChapterEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditChoiceEvent' oldEvent) (EditChoiceEvent' newEvent) =
    EditChoiceEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditExpertEvent' oldEvent) (EditExpertEvent' newEvent) =
    EditExpertEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditIntegrationEvent' oldEvent) (EditIntegrationEvent' newEvent) =
    EditIntegrationEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditKnowledgeModelEvent' oldEvent) (EditKnowledgeModelEvent' newEvent) =
    EditKnowledgeModelEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditMetricEvent' oldEvent) (EditMetricEvent' newEvent) =
    EditMetricEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditPhaseEvent' oldEvent) (EditPhaseEvent' newEvent) =
    EditPhaseEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditQuestionEvent' oldEvent) (EditQuestionEvent' newEvent) =
    EditQuestionEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditReferenceEvent' oldEvent) (EditReferenceEvent' newEvent) =
    EditReferenceEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent (EditTagEvent' oldEvent) (EditTagEvent' newEvent) =
    EditTagEvent' $ simpleSquashEvent oldEvent newEvent
  simpleSquashEvent _ newEvent = newEvent
