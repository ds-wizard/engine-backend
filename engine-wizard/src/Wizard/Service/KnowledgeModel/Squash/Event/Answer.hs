module Wizard.Service.KnowledgeModel.Squash.Event.Answer where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.EventLenses
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditAnswerEvent where
  isSimpleEventSquashApplicable = not . isChanged followUpUuids
  isReorderEventSquashApplicable previousEvent newEvent = previousEvent ^. entityUuid' == newEvent ^. entityUuid'
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditAnswerEvent
      { _editAnswerEventUuid = newEvent ^. uuid
      , _editAnswerEventParentUuid = newEvent ^. parentUuid
      , _editAnswerEventEntityUuid = newEvent ^. entityUuid
      , _editAnswerEventLabel = applyValue oldEvent newEvent label
      , _editAnswerEventAdvice = applyValue oldEvent newEvent advice
      , _editAnswerEventAnnotations = applyValue oldEvent newEvent annotations
      , _editAnswerEventFollowUpUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent followUpUuids
      , _editAnswerEventMetricMeasures = applyValue oldEvent newEvent metricMeasures
      , _editAnswerEventCreatedAt = newEvent ^. createdAt
      }
