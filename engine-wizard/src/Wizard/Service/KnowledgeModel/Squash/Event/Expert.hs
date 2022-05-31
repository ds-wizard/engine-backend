module Wizard.Service.KnowledgeModel.Squash.Event.Expert where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Expert.ExpertEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditExpertEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditExpertEvent
      { _editExpertEventUuid = newEvent ^. uuid
      , _editExpertEventParentUuid = newEvent ^. parentUuid
      , _editExpertEventEntityUuid = newEvent ^. entityUuid
      , _editExpertEventName = applyValue oldEvent newEvent name
      , _editExpertEventEmail = applyValue oldEvent newEvent email
      , _editExpertEventAnnotations = applyValue oldEvent newEvent annotations
      , _editExpertEventCreatedAt = newEvent ^. createdAt
      }
