module Wizard.Service.KnowledgeModel.Squash.Event.Tag where

import Shared.Model.Event.Tag.TagEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

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
      , createdAt = newEvent.createdAt
      }
