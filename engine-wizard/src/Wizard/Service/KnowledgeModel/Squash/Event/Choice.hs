module Wizard.Service.KnowledgeModel.Squash.Event.Choice where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Choice.ChoiceEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditChoiceEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditChoiceEvent
      { _editChoiceEventUuid = newEvent ^. uuid
      , _editChoiceEventParentUuid = newEvent ^. parentUuid
      , _editChoiceEventEntityUuid = newEvent ^. entityUuid
      , _editChoiceEventLabel = applyValue oldEvent newEvent label
      , _editChoiceEventAnnotations = applyValue oldEvent newEvent annotations
      , _editChoiceEventCreatedAt = newEvent ^. createdAt
      }
