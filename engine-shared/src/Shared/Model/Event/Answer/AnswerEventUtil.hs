module Shared.Model.Event.Answer.AnswerEventUtil where

import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Common.CommonUtil
import Shared.Model.Event.EventField

instance IsEmptyEvent EditAnswerEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.aLabel
      , isChangedValue event.advice
      , isChangedValue event.annotations
      , isChangedValue event.metricMeasures
      ]
