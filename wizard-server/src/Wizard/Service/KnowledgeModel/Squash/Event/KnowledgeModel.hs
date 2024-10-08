module Wizard.Service.KnowledgeModel.Squash.Event.KnowledgeModel where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEvent

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
      , annotations = applyValue oldEvent newEvent (.annotations)
      , chapterUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.chapterUuids)
      , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.tagUuids)
      , integrationUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.integrationUuids)
      , metricUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.metricUuids)
      , phaseUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.phaseUuids)
      , resourceCollectionUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.resourceCollectionUuids)
      , createdAt = oldEvent.createdAt
      }
