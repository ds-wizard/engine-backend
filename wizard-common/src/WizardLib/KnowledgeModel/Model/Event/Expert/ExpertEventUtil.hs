module WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEventUtil where

import WizardLib.KnowledgeModel.Model.Event.Common.CommonUtil
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEvent

instance IsEmptyEvent EditExpertEvent where
  isEmptyEvent event =
    or [isChangedValue event.name, isChangedValue event.email, isChangedValue event.annotations]
