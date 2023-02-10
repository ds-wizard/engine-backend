module Wizard.Service.KnowledgeModel.Squash.Event.Answer where

import qualified Data.UUID as U

import Shared.Model.Common.MapEntry
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.EventField
import Shared.Model.Event.EventLenses
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditAnswerEvent where
  isSimpleEventSquashApplicable = not . isChanged ((.followUpUuids) :: EditAnswerEvent -> EventField [U.UUID])
  isReorderEventSquashApplicable previousEvent newEvent = getEntityUuid previousEvent == getEntityUuid newEvent
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditAnswerEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , aLabel = applyValue oldEvent newEvent ((.aLabel) :: EditAnswerEvent -> EventField String)
      , advice = applyValue oldEvent newEvent ((.advice) :: EditAnswerEvent -> EventField (Maybe String))
      , annotations = applyValue oldEvent newEvent ((.annotations) :: EditAnswerEvent -> EventField [MapEntry String String])
      , followUpUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent ((.followUpUuids) :: EditAnswerEvent -> EventField [U.UUID])
      , metricMeasures = applyValue oldEvent newEvent ((.metricMeasures) :: EditAnswerEvent -> EventField [MetricMeasure])
      , createdAt = newEvent.createdAt
      }
