module Wizard.Service.KnowledgeModel.Squash.Event.Metric where

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField
import Shared.Model.Event.Metric.MetricEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditMetricEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditMetricEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , title = applyValue oldEvent newEvent ((.title) :: EditMetricEvent -> EventField String)
      , abbreviation = applyValue oldEvent newEvent ((.abbreviation) :: EditMetricEvent -> EventField (Maybe String))
      , description = applyValue oldEvent newEvent ((.description) :: EditMetricEvent -> EventField (Maybe String))
      , annotations = applyValue oldEvent newEvent ((.annotations) :: EditMetricEvent -> EventField [MapEntry String String])
      , createdAt = newEvent.createdAt
      }
