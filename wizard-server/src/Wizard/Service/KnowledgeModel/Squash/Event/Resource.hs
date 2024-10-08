module Wizard.Service.KnowledgeModel.Squash.Event.Resource where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEvent

instance SimpleEventSquash EditResourceCollectionEvent where
  isSimpleEventSquashApplicable = not . isChanged resourcePageUuids
  isReorderEventSquashApplicable previousEvent newEvent = getEntityUuid previousEvent == getEntityUuid newEvent
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditResourceCollectionEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , title = applyValue oldEvent newEvent (.title)
      , resourcePageUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.resourcePageUuids)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , createdAt = oldEvent.createdAt
      }

instance SimpleEventSquash EditResourcePageEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditResourcePageEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , title = applyValue oldEvent newEvent (.title)
      , content = applyValue oldEvent newEvent (.content)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , createdAt = oldEvent.createdAt
      }
