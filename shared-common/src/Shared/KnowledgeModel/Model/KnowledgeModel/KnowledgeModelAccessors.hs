module Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

-- -------------------
-- CHAPTERS ----------
-- -------------------
getChaptersForKmUuid :: KnowledgeModel -> [Chapter]
getChaptersForKmUuid km = foldl go [] km.chapterUuids
  where
    go acc chUuid =
      case M.lookup chUuid (getChaptersM km) of
        Just ch -> acc ++ [ch]
        Nothing -> acc

-- -------------------
-- QUESTIONS ---------
-- -------------------
getQuestionUuidsForChapterUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getQuestionUuidsForChapterUuid km chUuid =
  case M.lookup chUuid (getChaptersM km) of
    Just chapter -> chapter.questionUuids
    Nothing -> []

getQuestionsForChapterUuid :: KnowledgeModel -> U.UUID -> [Question]
getQuestionsForChapterUuid km chUuid =
  case M.lookup chUuid (getChaptersM km) of
    Just ch -> foldl go [] ch.questionUuids
    Nothing -> []
  where
    go acc qUuid =
      case M.lookup qUuid (getQuestionsM km) of
        Just q -> acc ++ [q]
        Nothing -> acc

getQuestionUuidsForAnswerUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getQuestionUuidsForAnswerUuid km ansUuid =
  case M.lookup ansUuid (getAnswersM km) of
    Just ans -> ans.followUpUuids
    Nothing -> []

getQuestionsForAnswerUuid :: KnowledgeModel -> U.UUID -> [Question]
getQuestionsForAnswerUuid km ansUuid =
  case M.lookup ansUuid (getAnswersM km) of
    Just ans -> foldl go [] ans.followUpUuids
    Nothing -> []
  where
    go acc qUuid =
      case M.lookup qUuid (getQuestionsM km) of
        Just q -> acc ++ [q]
        Nothing -> acc

getItemTemplateQuestionUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getItemTemplateQuestionUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (getQuestionsM km) of
    Just (ListQuestion' q) -> q.itemTemplateQuestionUuids
    _ -> []

getItemTemplateQuestionsForQuestionUuid :: KnowledgeModel -> U.UUID -> [Question]
getItemTemplateQuestionsForQuestionUuid km qUuid =
  case M.lookup qUuid (getQuestionsM km) of
    Just q -> foldl go [] (getItemTemplateQuestionUuids q)
    Nothing -> []
  where
    go acc itqUuid =
      case M.lookup itqUuid (getQuestionsM km) of
        Just itq -> acc ++ [itq]
        Nothing -> acc

-- -------------------
-- EXPERT ------------
-- -------------------
getExpertUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getExpertUuidsForQuestionUuid km questionUuid =
  maybe [] getExpertUuids . M.lookup questionUuid . getQuestionsM $ km

-- -------------------
-- REFERENCE ---------
-- -------------------
getReferenceUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getReferenceUuidsForQuestionUuid km questionUuid =
  maybe [] getReferenceUuids . M.lookup questionUuid . getQuestionsM $ km

-- -------------------
-- ANSWER ------------
-- -------------------
getAnswerUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getAnswerUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (getQuestionsM km) of
    Just (OptionsQuestion' q) -> q.answerUuids
    _ -> []

getAnswersForQuestionUuid :: KnowledgeModel -> U.UUID -> [Answer]
getAnswersForQuestionUuid km questionUuid =
  case M.lookup questionUuid (getQuestionsM km) of
    Just (OptionsQuestion' q) -> foldl go [] q.answerUuids
    _ -> []
  where
    go acc ansUuid =
      case M.lookup ansUuid (getAnswersM km) of
        Just ans -> acc ++ [ans]
        Nothing -> acc

-- -------------------
-- CHOICE ------------
-- -------------------
getChoiceUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getChoiceUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (getQuestionsM km) of
    Just (MultiChoiceQuestion' q) -> q.choiceUuids
    _ -> []

getChoicesForQuestionUuid :: KnowledgeModel -> U.UUID -> [Choice]
getChoicesForQuestionUuid km questionUuid =
  case M.lookup questionUuid (getQuestionsM km) of
    Just (MultiChoiceQuestion' q) -> foldl go [] q.choiceUuids
    _ -> []
  where
    go acc choiceUuid =
      case M.lookup choiceUuid (getChoicesM km) of
        Just choice -> acc ++ [choice]
        Nothing -> acc

-- -------------------
-- METRICS ----------
-- -------------------
getMetricsForKmUuid :: KnowledgeModel -> [Metric]
getMetricsForKmUuid km = foldl go [] km.metricUuids
  where
    go acc mtrUuid =
      case M.lookup mtrUuid (getMetricsM km) of
        Just mtr -> acc ++ [mtr]
        Nothing -> acc

-- -------------------
-- PHASES ------------
-- -------------------
getPhasesForKmUuid :: KnowledgeModel -> [Phase]
getPhasesForKmUuid km = foldl go [] km.phaseUuids
  where
    go acc phsUuid =
      case M.lookup phsUuid (getPhasesM km) of
        Just phs -> acc ++ [phs]
        Nothing -> acc

-- -------------------
-- RESOURCE PAGES ----
-- -------------------
getResourcePagesUuidsForResourceCollectionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getResourcePagesUuidsForResourceCollectionUuid km resourceCollectionUuid =
  maybe [] (.resourcePageUuids) . M.lookup resourceCollectionUuid . getResourceCollectionsM $ km
