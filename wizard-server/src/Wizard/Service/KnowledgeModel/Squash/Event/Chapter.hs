module Wizard.Service.KnowledgeModel.Squash.Event.Chapter where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.Event.EventLenses

instance SimpleEventSquash EditChapterEvent where
  isSimpleEventSquashApplicable = not . isChanged questionUuids
  isReorderEventSquashApplicable previousEvent newEvent = getEntityUuid previousEvent == getEntityUuid newEvent
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditChapterEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , title = applyValue oldEvent newEvent (.title)
      , text = applyValue oldEvent newEvent (.text)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , questionUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.questionUuids)
      , createdAt = oldEvent.createdAt
      }
