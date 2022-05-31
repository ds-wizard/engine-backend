module Wizard.Service.KnowledgeModel.Squash.Event.Phase where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Phase.PhaseEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditPhaseEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditPhaseEvent
      { _editPhaseEventUuid = newEvent ^. uuid
      , _editPhaseEventParentUuid = newEvent ^. parentUuid
      , _editPhaseEventEntityUuid = newEvent ^. entityUuid
      , _editPhaseEventTitle = applyValue oldEvent newEvent title
      , _editPhaseEventDescription = applyValue oldEvent newEvent description
      , _editPhaseEventAnnotations = applyValue oldEvent newEvent annotations
      , _editPhaseEventCreatedAt = newEvent ^. createdAt
      }
