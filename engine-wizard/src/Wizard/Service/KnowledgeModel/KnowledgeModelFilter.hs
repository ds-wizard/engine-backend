module Wizard.Service.KnowledgeModel.KnowledgeModelFilter where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete

filterKnowledgeModel :: [U.UUID] -> KnowledgeModel -> KnowledgeModel
filterKnowledgeModel [] km = km
filterKnowledgeModel selectedQuestionTagUuids km = removeOrphanUuids . filterTagUuids . filterTags . filterChapters $ km
  where
    filterChapters km = foldl (filterChapter selectedQuestionTagUuids) km (getChaptersL km)
    filterTags km = setTagsM km $ M.filterWithKey (\tUuid _ -> tUuid `elem` selectedQuestionTagUuids) (getTagsM km)
    filterTagUuids km = km {tagUuids = filter (`elem` selectedQuestionTagUuids) km.tagUuids} :: KnowledgeModel

filterChapter :: [U.UUID] -> KnowledgeModel -> Chapter -> KnowledgeModel
filterChapter selectedQuestionTagUuids km chapter =
  let qs = getQuestionsForChapterUuid km chapter.uuid
   in foldl (filterQuestion selectedQuestionTagUuids) km qs

filterQuestion :: [U.UUID] -> KnowledgeModel -> Question -> KnowledgeModel
filterQuestion selectedQuestionTagUuids km q =
  case filter (`elem` selectedQuestionTagUuids) (getTagUuids q) of
    [] -> deleteQuestion km (getUuid q)
    filteredQuestionTagUuids -> applyToQuestion filteredQuestionTagUuids q . applyToChildren q $ km
  where
    applyToQuestion filteredQuestionTagUuids q km =
      let updatedQuestion = setTagUuids q filteredQuestionTagUuids
       in setQuestionsM km (M.insert (getUuid q) updatedQuestion (getQuestionsM km))
    applyToChildren (OptionsQuestion' q) km = foldl go km q.answerUuids
      where
        go km ansUuid =
          let qs = getQuestionsForAnswerUuid km ansUuid
           in foldl (filterQuestion selectedQuestionTagUuids) km qs
    applyToChildren (ListQuestion' q) km =
      let qs = getItemTemplateQuestionsForQuestionUuid km q.uuid
       in foldl (filterQuestion selectedQuestionTagUuids) km qs
    applyToChildren q km = km

removeOrphanUuids :: KnowledgeModel -> KnowledgeModel
removeOrphanUuids = removeOrphanUuidInQuestions . removeOrphanUuidInChapters
  where
    removeOrphanUuidInChapters km = foldl removeOrphanUuidInChapter km (getChaptersL km)
    removeOrphanUuidInQuestions km = foldl removeOrphanUuidInQuestion km (getQuestionsL km)

removeOrphanUuidInChapter :: KnowledgeModel -> Chapter -> KnowledgeModel
removeOrphanUuidInChapter km ch = setChaptersM km (M.insert (getUuid ch) (ch {questionUuids = newQuestionUuids}) (getChaptersM km))
  where
    newQuestionUuids =
      fmap getUuid . mapMaybe (\qUuid -> M.lookup qUuid (getQuestionsM km)) $ ch.questionUuids

removeOrphanUuidInQuestion :: KnowledgeModel -> Question -> KnowledgeModel
removeOrphanUuidInQuestion km q = setQuestionsM km (M.insert (getUuid q) (applyToChildren q) (getQuestionsM km))
  where
    applyToChildren (OptionsQuestion' q) = OptionsQuestion' $ q {answerUuids = newAnswerUuids}
      where
        newAnswerUuids =
          fmap getUuid . mapMaybe (\ansUuid -> M.lookup ansUuid (getAnswersM km)) $ q.answerUuids
    applyToChildren (ListQuestion' q) = ListQuestion' $ q {itemTemplateQuestionUuids = newQuestionUuids}
      where
        newQuestionUuids =
          fmap getUuid . mapMaybe (\qUuid -> M.lookup qUuid (getQuestionsM km)) $ q.itemTemplateQuestionUuids
    applyToChildren q = q
