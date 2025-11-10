module Wizard.Service.KnowledgeModel.Squash.Event.Metric where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Metric.MetricEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditMetricEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditMetricEvent'
        EditMetricEvent
          { title = applyValue oldContent newContent (.title)
          , abbreviation = applyValue oldContent newContent (.abbreviation)
          , description = applyValue oldContent newContent (.description)
          , annotations = applyValue oldContent newContent (.annotations)
          }
