module Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M

import LensesConfig
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddKnowledgeModelEvent KnowledgeModel where
  createEntity e =
    KnowledgeModel
      { _knowledgeModelUuid = e ^. entityUuid
      , _knowledgeModelAnnotations = e ^. annotations
      , _knowledgeModelChapterUuids = []
      , _knowledgeModelTagUuids = []
      , _knowledgeModelIntegrationUuids = []
      , _knowledgeModelMetricUuids = []
      , _knowledgeModelPhaseUuids = []
      , _knowledgeModelEntities =
          KnowledgeModelEntities
            { _knowledgeModelEntitiesChapters = M.empty
            , _knowledgeModelEntitiesQuestions = M.empty
            , _knowledgeModelEntitiesAnswers = M.empty
            , _knowledgeModelEntitiesChoices = M.empty
            , _knowledgeModelEntitiesExperts = M.empty
            , _knowledgeModelEntitiesReferences = M.empty
            , _knowledgeModelEntitiesIntegrations = M.empty
            , _knowledgeModelEntitiesTags = M.empty
            , _knowledgeModelEntitiesMetrics = M.empty
            , _knowledgeModelEntitiesPhases = M.empty
            }
      }

instance EditEntity EditKnowledgeModelEvent KnowledgeModel where
  editEntity e =
    applyPhaseUuids . applyMetricUuids . applyIntegrationUuids . applyTagUuids . applyChapterUuids . applyAnnotations
    where
      applyAnnotations km = applyValue (e ^. annotations) km annotations
      applyChapterUuids km = applyValue (e ^. chapterUuids) km chapterUuids
      applyTagUuids km = applyValue (e ^. tagUuids) km tagUuids
      applyIntegrationUuids km = applyValue (e ^. integrationUuids) km integrationUuids
      applyMetricUuids km = applyValue (e ^. metricUuids) km metricUuids
      applyPhaseUuids km = applyValue (e ^. phaseUuids) km phaseUuids
