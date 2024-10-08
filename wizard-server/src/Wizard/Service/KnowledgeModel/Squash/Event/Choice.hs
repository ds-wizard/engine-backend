module Wizard.Service.KnowledgeModel.Squash.Event.Choice where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent

instance SimpleEventSquash EditChoiceEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditChoiceEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , aLabel = applyValue oldEvent newEvent (.aLabel)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , createdAt = oldEvent.createdAt
      }
