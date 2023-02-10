module Wizard.Service.KnowledgeModel.Squash.Event.Phase where

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField
import Shared.Model.Event.Phase.PhaseEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditPhaseEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditPhaseEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , title = applyValue oldEvent newEvent ((.title) :: EditPhaseEvent -> EventField String)
      , description = applyValue oldEvent newEvent ((.description) :: EditPhaseEvent -> EventField (Maybe String))
      , annotations = applyValue oldEvent newEvent ((.annotations) :: EditPhaseEvent -> EventField [MapEntry String String])
      , createdAt = newEvent.createdAt
      }
