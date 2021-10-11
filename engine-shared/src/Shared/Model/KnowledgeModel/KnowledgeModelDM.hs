module Shared.Model.KnowledgeModel.KnowledgeModelDM where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Shared.Model.KnowledgeModel.KnowledgeModel

defaultKnowledgeModel :: KnowledgeModel
defaultKnowledgeModel =
  KnowledgeModel
    { _knowledgeModelUuid = U.nil
    , _knowledgeModelAnnotations = M.empty
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
