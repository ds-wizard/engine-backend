module Wizard.Model.KnowledgeModel.KnowledgeModelAccessors where

import Control.Lens
import qualified Data.Map as M
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.LensesConfig
import Wizard.Model.KnowledgeModel.KnowledgeModelLenses

-- -------------------
-- CHAPTERS ----------
-- -------------------
getChaptersForKmUuid :: KnowledgeModel -> [Chapter]
getChaptersForKmUuid km = foldl go [] (km ^. chapterUuids)
  where
    go acc chUuid =
      case M.lookup (chUuid) (km ^. chaptersM) of
        Just ch -> acc ++ [ch]
        Nothing -> acc

-- -------------------
-- QUESTIONS ---------
-- -------------------
getQuestionUuidsForChapterUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getQuestionUuidsForChapterUuid km chUuid =
  case M.lookup chUuid (km ^. chaptersM) of
    Just chapter -> chapter ^. questionUuids
    Nothing -> []

getQuestionsForChapterUuid :: KnowledgeModel -> U.UUID -> [Question]
getQuestionsForChapterUuid km chUuid =
  case M.lookup chUuid (km ^. chaptersM) of
    Just ch -> foldl go [] (ch ^. questionUuids)
    Nothing -> []
  where
    go acc qUuid =
      case M.lookup (qUuid) (km ^. questionsM) of
        Just q -> acc ++ [q]
        Nothing -> acc

getQuestionUuidsForAnswerUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getQuestionUuidsForAnswerUuid km ansUuid =
  case M.lookup ansUuid (km ^. answersM) of
    Just ans -> ans ^. followUpUuids
    Nothing -> []

getQuestionsForAnswerUuid :: KnowledgeModel -> U.UUID -> [Question]
getQuestionsForAnswerUuid km ansUuid =
  case M.lookup ansUuid (km ^. answersM) of
    Just ans -> foldl go [] (ans ^. followUpUuids)
    Nothing -> []
  where
    go acc qUuid =
      case M.lookup (qUuid) (km ^. questionsM) of
        Just q -> acc ++ [q]
        Nothing -> acc

getItemTemplateQuestionUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getItemTemplateQuestionUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (km ^. questionsM) of
    Just (ListQuestion' q) -> q ^. itemTemplateQuestionUuids
    Nothing -> []

getItemTemplateQuestionsForQuestionUuid :: KnowledgeModel -> U.UUID -> [Question]
getItemTemplateQuestionsForQuestionUuid km qUuid =
  case M.lookup qUuid (km ^. questionsM) of
    Just q -> foldl go [] (q ^. itemTemplateQuestionUuids')
    Nothing -> []
  where
    go acc itqUuid =
      case M.lookup (itqUuid) (km ^. questionsM) of
        Just itq -> acc ++ [itq]
        Nothing -> acc

-- -------------------
-- EXPERT ------------
-- -------------------
getExpertUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getExpertUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (km ^. questionsM) of
    Just question -> question ^. expertUuids'
    Nothing -> []

-- -------------------
-- REFERENCE ---------
-- -------------------
getReferenceUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getReferenceUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (km ^. questionsM) of
    Just question -> question ^. referenceUuids'
    Nothing -> []

-- -------------------
-- ANSWER ------------
-- -------------------
getAnswerUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getAnswerUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (km ^. questionsM) of
    Just (OptionsQuestion' q) -> q ^. answerUuids
    _ -> []
