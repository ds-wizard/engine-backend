module Wizard.Service.KnowledgeModel.Squash.Event.Phase where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.Phase.PhaseEvent

instance SimpleEventSquash EditPhaseEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditPhaseEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , title = applyValue oldEvent newEvent (.title)
      , description = applyValue oldEvent newEvent (.description)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , createdAt = newEvent.createdAt
      }
