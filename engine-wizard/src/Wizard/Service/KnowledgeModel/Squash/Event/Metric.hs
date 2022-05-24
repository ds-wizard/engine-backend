module Wizard.Service.KnowledgeModel.Squash.Event.Metric where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Metric.MetricEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditMetricEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditMetricEvent
      { _editMetricEventUuid = newEvent ^. uuid
      , _editMetricEventParentUuid = newEvent ^. parentUuid
      , _editMetricEventEntityUuid = newEvent ^. entityUuid
      , _editMetricEventTitle = applyValue oldEvent newEvent title
      , _editMetricEventAbbreviation = applyValue oldEvent newEvent abbreviation
      , _editMetricEventDescription = applyValue oldEvent newEvent description
      , _editMetricEventAnnotations = applyValue oldEvent newEvent annotations
      , _editMetricEventCreatedAt = newEvent ^. createdAt
      }
