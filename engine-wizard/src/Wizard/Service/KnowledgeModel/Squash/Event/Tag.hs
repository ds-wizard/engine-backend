module Wizard.Service.KnowledgeModel.Squash.Event.Tag where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Tag.TagEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditTagEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditTagEvent
      { _editTagEventUuid = newEvent ^. uuid
      , _editTagEventParentUuid = newEvent ^. parentUuid
      , _editTagEventEntityUuid = newEvent ^. entityUuid
      , _editTagEventName = applyValue oldEvent newEvent name
      , _editTagEventDescription = applyValue oldEvent newEvent description
      , _editTagEventColor = applyValue oldEvent newEvent color
      , _editTagEventAnnotations = applyValue oldEvent newEvent annotations
      , _editTagEventCreatedAt = newEvent ^. createdAt
      }
