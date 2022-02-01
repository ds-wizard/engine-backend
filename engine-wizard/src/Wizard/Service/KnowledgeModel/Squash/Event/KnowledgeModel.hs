module Wizard.Service.KnowledgeModel.Squash.Event.KnowledgeModel where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.EventField
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditKnowledgeModelEvent where
  isSimpleEventSquashApplicable event =
    not $
    isChanged chapterUuids event ||
    isChanged tagUuids event ||
    isChanged integrationUuids event || isChanged metricUuids event || isChanged phaseUuids event
  isTypeChanged _ _ = False
  simpleSquashEvent oldEvent newEvent =
    EditKnowledgeModelEvent
      { _editKnowledgeModelEventUuid = newEvent ^. uuid
      , _editKnowledgeModelEventParentUuid = newEvent ^. parentUuid
      , _editKnowledgeModelEventEntityUuid = newEvent ^. entityUuid
      , _editKnowledgeModelEventAnnotations = applyValue oldEvent newEvent annotations
      , _editKnowledgeModelEventChapterUuids = NothingChanged
      , _editKnowledgeModelEventTagUuids = NothingChanged
      , _editKnowledgeModelEventIntegrationUuids = NothingChanged
      , _editKnowledgeModelEventMetricUuids = NothingChanged
      , _editKnowledgeModelEventPhaseUuids = NothingChanged
      , _editKnowledgeModelEventCreatedAt = newEvent ^. createdAt
      }
