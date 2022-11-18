module Wizard.Service.KnowledgeModel.Squash.Event.Expert where

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField
import Shared.Model.Event.Expert.ExpertEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditExpertEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditExpertEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , name = applyValue oldEvent newEvent (name :: EditExpertEvent -> EventField String)
      , email = applyValue oldEvent newEvent (email :: EditExpertEvent -> EventField String)
      , annotations = applyValue oldEvent newEvent (annotations :: EditExpertEvent -> EventField [MapEntry String String])
      , createdAt = newEvent.createdAt
      }
