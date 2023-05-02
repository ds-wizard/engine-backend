module WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEventUtil where

import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent
import WizardLib.KnowledgeModel.Model.Event.Common.CommonUtil
import WizardLib.KnowledgeModel.Model.Event.EventField

instance IsEmptyEvent EditAnswerEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.aLabel
      , isChangedValue event.advice
      , isChangedValue event.annotations
      , isChangedValue event.metricMeasures
      ]
