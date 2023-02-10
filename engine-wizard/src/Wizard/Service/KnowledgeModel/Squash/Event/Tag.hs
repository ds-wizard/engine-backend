module Wizard.Service.KnowledgeModel.Squash.Event.Tag where

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField
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
      , name = applyValue oldEvent newEvent ((.name) :: EditTagEvent -> EventField String)
      , description = applyValue oldEvent newEvent ((.description) :: EditTagEvent -> EventField (Maybe String))
      , color = applyValue oldEvent newEvent ((.color) :: EditTagEvent -> EventField String)
      , annotations = applyValue oldEvent newEvent ((.annotations) :: EditTagEvent -> EventField [MapEntry String String])
      , createdAt = newEvent.createdAt
      }
