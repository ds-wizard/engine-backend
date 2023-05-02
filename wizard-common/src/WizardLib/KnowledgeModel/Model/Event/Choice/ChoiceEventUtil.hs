module WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEventUtil where

import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent
import WizardLib.KnowledgeModel.Model.Event.Common.CommonUtil
import WizardLib.KnowledgeModel.Model.Event.EventField

instance IsEmptyEvent EditChoiceEvent where
  isEmptyEvent event = or [isChangedValue event.aLabel, isChangedValue event.annotations]
