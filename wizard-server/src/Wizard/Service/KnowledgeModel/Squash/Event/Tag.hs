module Wizard.Service.KnowledgeModel.Squash.Event.Tag where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent

instance SimpleEventSquash EditTagEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditTagEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , name = applyValue oldEvent newEvent (.name)
      , description = applyValue oldEvent newEvent (.description)
      , color = applyValue oldEvent newEvent (.color)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , createdAt = oldEvent.createdAt
      }
