module Shared.Model.KnowledgeModel.KnowledgeModelDM where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Shared.Model.KnowledgeModel.KnowledgeModel

defaultKnowledgeModel :: KnowledgeModel
defaultKnowledgeModel =
  KnowledgeModel
    { uuid = U.nil
    , annotations = []
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
