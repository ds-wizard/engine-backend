module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEventUtil where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Common.CommonUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance IsEmptyEvent EditChoiceEvent where
  isEmptyEvent event = or [isChangedValue event.aLabel, isChangedValue event.annotations]
