module Wizard.Service.KnowledgeModel.Squash.Event.Chapter where

import qualified Data.UUID as U

import Shared.Model.Common.MapEntry
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.EventField
import Shared.Model.Event.EventLenses
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditChapterEvent where
  isSimpleEventSquashApplicable = not . isChanged questionUuids
  isReorderEventSquashApplicable previousEvent newEvent = getEntityUuid previousEvent == getEntityUuid newEvent
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditChapterEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , title = applyValue oldEvent newEvent ((.title) :: EditChapterEvent -> EventField String)
      , text = applyValue oldEvent newEvent ((.text) :: EditChapterEvent -> EventField (Maybe String))
      , annotations = applyValue oldEvent newEvent ((.annotations) :: EditChapterEvent -> EventField [MapEntry String String])
      , questionUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (questionUuids :: EditChapterEvent -> EventField [U.UUID])
      , createdAt = newEvent.createdAt
      }
