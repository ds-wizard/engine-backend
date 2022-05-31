module Wizard.Service.KnowledgeModel.Squash.Event.KnowledgeModel where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditKnowledgeModelEvent where
  isSimpleEventSquashApplicable event =
    not $
    isChanged chapterUuids event ||
    isChanged tagUuids event ||
    isChanged integrationUuids event || isChanged metricUuids event || isChanged phaseUuids event
  isReorderEventSquashApplicable _ _ = True
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditKnowledgeModelEvent
      { _editKnowledgeModelEventUuid = newEvent ^. uuid
      , _editKnowledgeModelEventParentUuid = newEvent ^. parentUuid
      , _editKnowledgeModelEventEntityUuid = newEvent ^. entityUuid
      , _editKnowledgeModelEventAnnotations = applyValue oldEvent newEvent annotations
      , _editKnowledgeModelEventChapterUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent chapterUuids
      , _editKnowledgeModelEventTagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent tagUuids
      , _editKnowledgeModelEventIntegrationUuids =
          applyValueIfSameEntity mPreviousEvent oldEvent newEvent integrationUuids
      , _editKnowledgeModelEventMetricUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent metricUuids
      , _editKnowledgeModelEventPhaseUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent phaseUuids
      , _editKnowledgeModelEventCreatedAt = newEvent ^. createdAt
      }
