module Wizard.Service.KnowledgeModel.Squash.Event.Event where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
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
import Wizard.Service.KnowledgeModel.Squash.Event.Resource ()
import Wizard.Service.KnowledgeModel.Squash.Event.Tag ()

isSimpleEventSquashApplicable' :: KnowledgeModelEvent -> Bool
isSimpleEventSquashApplicable' event =
  case event.content of
    (EditAnswerEvent' event) -> isSimpleEventSquashApplicable event
    (EditChapterEvent' event) -> isSimpleEventSquashApplicable event
    (EditChoiceEvent' event) -> isSimpleEventSquashApplicable event
    (EditExpertEvent' event) -> isSimpleEventSquashApplicable event
    (EditIntegrationEvent' event) -> isSimpleEventSquashApplicable event
    (EditKnowledgeModelEvent' event) -> isSimpleEventSquashApplicable event
    (EditMetricEvent' event) -> isSimpleEventSquashApplicable event
    (EditPhaseEvent' event) -> isSimpleEventSquashApplicable event
    (EditQuestionEvent' event) -> isSimpleEventSquashApplicable event
    (EditReferenceEvent' event) -> isSimpleEventSquashApplicable event
    (EditTagEvent' event) -> isSimpleEventSquashApplicable event
    (EditResourceCollectionEvent' event) -> isSimpleEventSquashApplicable event
    (EditResourcePageEvent' event) -> isSimpleEventSquashApplicable event
    _ -> False

--  --------------------------------------
isReorderEventSquashApplicable' :: KnowledgeModelEvent -> KnowledgeModelEvent -> Bool
isReorderEventSquashApplicable' previousEvent newEvent =
  case (previousEvent.content, newEvent.content) of
    (EditAnswerEvent' previousContent, EditAnswerEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditChapterEvent' previousContent, EditChapterEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditChoiceEvent' previousContent, EditChoiceEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditExpertEvent' previousContent, EditExpertEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditIntegrationEvent' previousContent, EditIntegrationEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditKnowledgeModelEvent' previousContent, EditKnowledgeModelEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditMetricEvent' previousContent, EditMetricEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditPhaseEvent' previousContent, EditPhaseEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditQuestionEvent' previousContent, EditQuestionEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditReferenceEvent' previousContent, EditReferenceEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditTagEvent' previousContent, EditTagEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditResourceCollectionEvent' previousContent, EditResourceCollectionEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (EditResourcePageEvent' previousContent, EditResourcePageEvent' newContent) -> isReorderEventSquashApplicable (previousEvent, previousContent) (newEvent, newContent)
    (_, _) -> False

--  --------------------------------------
isTypeChanged' :: KnowledgeModelEvent -> KnowledgeModelEvent -> Bool
isTypeChanged' oldEvent newEvent =
  case (oldEvent.content, newEvent.content) of
    (EditIntegrationEvent' oldContent, EditIntegrationEvent' newContent) -> isTypeChanged oldContent newContent
    (EditQuestionEvent' oldContent, EditQuestionEvent' newContent) -> isTypeChanged oldContent newContent
    (EditReferenceEvent' oldContent, EditReferenceEvent' newContent) -> isTypeChanged oldContent newContent
    (_, _) -> False

--  --------------------------------------
simpleSquashEvent' :: Maybe KnowledgeModelEvent -> KnowledgeModelEvent -> KnowledgeModelEvent -> KnowledgeModelEvent
simpleSquashEvent' mPreviousEvent oldEvent newEvent =
  case (oldEvent.content, newEvent.content) of
    (EditAnswerEvent' oldContent, EditAnswerEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditChapterEvent' oldContent, EditChapterEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditChoiceEvent' oldContent, EditChoiceEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditExpertEvent' oldContent, EditExpertEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditIntegrationEvent' oldContent, EditIntegrationEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditKnowledgeModelEvent' oldContent, EditKnowledgeModelEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditMetricEvent' oldContent, EditMetricEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditPhaseEvent' oldContent, EditPhaseEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditQuestionEvent' oldContent, EditQuestionEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditReferenceEvent' oldContent, EditReferenceEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditTagEvent' oldContent, EditTagEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditResourceCollectionEvent' oldContent, EditResourceCollectionEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (EditResourcePageEvent' oldContent, EditResourcePageEvent' newContent) -> simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent)
    (_, _) -> newEvent
