module Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel where

import qualified Data.Map.Strict as M

import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance CreateEntity AddKnowledgeModelEvent KnowledgeModel where
  createEntity e =
    KnowledgeModel
      { uuid = e.entityUuid
      , annotations = e.annotations
      , chapterUuids = []
      , tagUuids = []
      , integrationUuids = []
      , metricUuids = []
      , phaseUuids = []
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
            }
      }

instance EditEntity EditKnowledgeModelEvent KnowledgeModel where
  editEntity event entity =
    entity
      { annotations = applyValue entity.annotations event.annotations
      , chapterUuids = applyValue entity.chapterUuids event.chapterUuids
      , tagUuids = applyValue entity.tagUuids event.tagUuids
      , integrationUuids = applyValue entity.integrationUuids event.integrationUuids
      , metricUuids = applyValue entity.metricUuids event.metricUuids
      , phaseUuids = applyValue entity.phaseUuids event.phaseUuids
      }
