module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEventUtil where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Common.CommonUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance IsEmptyEvent EditAnswerEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.aLabel
      , isChangedValue event.advice
      , isChangedValue event.annotations
      , isChangedValue event.metricMeasures
      ]
