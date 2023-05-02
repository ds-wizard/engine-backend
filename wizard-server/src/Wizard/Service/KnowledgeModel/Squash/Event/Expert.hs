module Wizard.Service.KnowledgeModel.Squash.Event.Expert where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEvent

instance SimpleEventSquash EditExpertEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditExpertEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , name = applyValue oldEvent newEvent (.name)
      , email = applyValue oldEvent newEvent (.email)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , createdAt = newEvent.createdAt
      }
