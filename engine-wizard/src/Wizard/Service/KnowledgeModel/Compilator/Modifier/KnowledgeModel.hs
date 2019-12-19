module Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel where

import Control.Lens ((^.))
import qualified Data.Map as M

import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.LensesConfig
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddKnowledgeModelEvent KnowledgeModel where
  createEntity e =
    KnowledgeModel
      { _knowledgeModelUuid = e ^. entityUuid
      , _knowledgeModelName = e ^. name
      , _knowledgeModelChapterUuids = []
      , _knowledgeModelTagUuids = []
      , _knowledgeModelIntegrationUuids = []
      , _knowledgeModelEntities =
          KnowledgeModelEntities
            { _knowledgeModelEntitiesChapters = M.empty
            , _knowledgeModelEntitiesQuestions = M.empty
            , _knowledgeModelEntitiesAnswers = M.empty
            , _knowledgeModelEntitiesExperts = M.empty
            , _knowledgeModelEntitiesReferences = M.empty
            , _knowledgeModelEntitiesIntegrations = M.empty
            , _knowledgeModelEntitiesTags = M.empty
            }
      }

instance EditEntity EditKnowledgeModelEvent KnowledgeModel where
  editEntity e = applyIntegrationUuids . applyTagUuids . applyChapterUuids . applyName
    where
      applyName km = applyValue (e ^. name) km name
      applyChapterUuids km = applyValue (e ^. chapterUuids) km chapterUuids
      applyTagUuids km = applyValue (e ^. tagUuids) km tagUuids
      applyIntegrationUuids km = applyValue (e ^. integrationUuids) km integrationUuids
