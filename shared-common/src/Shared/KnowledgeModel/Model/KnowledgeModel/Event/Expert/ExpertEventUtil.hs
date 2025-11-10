module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEventUtil where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Common.CommonUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance IsEmptyEvent EditExpertEvent where
  isEmptyEvent event =
    or [isChangedValue event.name, isChangedValue event.email, isChangedValue event.annotations]
