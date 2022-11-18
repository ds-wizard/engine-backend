module Wizard.Service.KnowledgeModel.Squash.Event.Choice where

import Shared.Model.Common.MapEntry
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.EventField
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditChoiceEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditChoiceEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , aLabel = applyValue oldEvent newEvent (aLabel :: EditChoiceEvent -> EventField String)
      , annotations = applyValue oldEvent newEvent (annotations :: EditChoiceEvent -> EventField [MapEntry String String])
      , createdAt = newEvent.createdAt
      }
