module Wizard.Service.KnowledgeModel.Squash.Event.Answer where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.EventField
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditAnswerEvent where
  isSimpleEventSquashApplicable = not . isChanged followUpUuids
  isTypeChanged _ _ = False
  simpleSquashEvent oldEvent newEvent =
    EditAnswerEvent
      { _editAnswerEventUuid = newEvent ^. uuid
      , _editAnswerEventParentUuid = newEvent ^. parentUuid
      , _editAnswerEventEntityUuid = newEvent ^. entityUuid
      , _editAnswerEventLabel = applyValue oldEvent newEvent label
      , _editAnswerEventAdvice = applyValue oldEvent newEvent advice
      , _editAnswerEventAnnotations = applyValue oldEvent newEvent annotations
      , _editAnswerEventFollowUpUuids = NothingChanged
      , _editAnswerEventMetricMeasures = applyValue oldEvent newEvent metricMeasures
      , _editAnswerEventCreatedAt = newEvent ^. createdAt
      }
