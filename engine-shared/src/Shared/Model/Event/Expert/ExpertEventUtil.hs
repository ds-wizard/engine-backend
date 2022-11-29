module Shared.Model.Event.Expert.ExpertEventUtil where

import Shared.Model.Event.Common.CommonUtil
import Shared.Model.Event.EventField
import Shared.Model.Event.Expert.ExpertEvent

instance IsEmptyEvent EditExpertEvent where
  isEmptyEvent event =
    or [isChangedValue event.name, isChangedValue event.email, isChangedValue event.annotations]
