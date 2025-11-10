module Wizard.Service.KnowledgeModel.Squash.Event.KnowledgeModel where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
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
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditKnowledgeModelEvent'
        EditKnowledgeModelEvent
          { annotations = applyValue oldContent newContent (.annotations)
          , chapterUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.chapterUuids)
          , tagUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.tagUuids)
          , integrationUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.integrationUuids)
          , metricUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.metricUuids)
          , phaseUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.phaseUuids)
          , resourceCollectionUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.resourceCollectionUuids)
          }
