module Wizard.Service.KnowledgeModel.Squash.Event.KnowledgeModel where

import qualified Data.UUID as U

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditKnowledgeModelEvent where
  isSimpleEventSquashApplicable event =
    not $
      isChanged chapterUuids event
        || isChanged tagUuids event
        || isChanged integrationUuids event
        || isChanged metricUuids event
        || isChanged phaseUuids event
  isReorderEventSquashApplicable _ _ = True
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditKnowledgeModelEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , annotations = applyValue oldEvent newEvent ((.annotations) :: EditKnowledgeModelEvent -> EventField [MapEntry String String])
      , chapterUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (chapterUuids :: EditKnowledgeModelEvent -> EventField [U.UUID])
      , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent ((.tagUuids) :: EditKnowledgeModelEvent -> EventField [U.UUID])
      , integrationUuids =
          applyValueIfSameEntity mPreviousEvent oldEvent newEvent (integrationUuids :: EditKnowledgeModelEvent -> EventField [U.UUID])
      , metricUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (metricUuids :: EditKnowledgeModelEvent -> EventField [U.UUID])
      , phaseUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (phaseUuids :: EditKnowledgeModelEvent -> EventField [U.UUID])
      , createdAt = newEvent.createdAt
      }
