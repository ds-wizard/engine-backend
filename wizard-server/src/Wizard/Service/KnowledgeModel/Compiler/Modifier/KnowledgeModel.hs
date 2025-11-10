module Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel where

import qualified Data.Map.Strict as M

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddKnowledgeModelEvent KnowledgeModel where
  createEntity event content =
    KnowledgeModel
      { uuid = event.entityUuid
      , annotations = content.annotations
      , chapterUuids = []
      , tagUuids = []
      , integrationUuids = []
      , metricUuids = []
      , phaseUuids = []
      , resourceCollectionUuids = []
      , entities =
          KnowledgeModelEntities
            { chapters = M.empty
            , questions = M.empty
            , answers = M.empty
            , choices = M.empty
            , experts = M.empty
            , references = M.empty
            , integrations = M.empty
            , tags = M.empty
            , metrics = M.empty
            , phases = M.empty
            , resourceCollections = M.empty
            , resourcePages = M.empty
            }
      }

instance EditEntity EditKnowledgeModelEvent KnowledgeModel where
  editEntity event content entity =
    entity
      { annotations = applyValue entity.annotations content.annotations
      , chapterUuids = applyValue entity.chapterUuids content.chapterUuids
      , tagUuids = applyValue entity.tagUuids content.tagUuids
      , integrationUuids = applyValue entity.integrationUuids content.integrationUuids
      , metricUuids = applyValue entity.metricUuids content.metricUuids
      , phaseUuids = applyValue entity.phaseUuids content.phaseUuids
      , resourceCollectionUuids = applyValue entity.resourceCollectionUuids content.resourceCollectionUuids
      }
