module Wizard.Service.KnowledgeModel.Squash.Event.Metric where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.Metric.MetricEvent

instance SimpleEventSquash EditMetricEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditMetricEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , title = applyValue oldEvent newEvent (.title)
      , abbreviation = applyValue oldEvent newEvent (.abbreviation)
      , description = applyValue oldEvent newEvent (.description)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , createdAt = oldEvent.createdAt
      }
