module Shared.Model.Event.Choice.ChoiceEventUtil where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.Common.CommonUtil
import Shared.Model.Event.EventField

instance IsEmptyEvent EditChoiceEvent where
  isEmptyEvent event = or [isChangedValue $ event ^. label, isChangedValue $ event ^. annotations]
