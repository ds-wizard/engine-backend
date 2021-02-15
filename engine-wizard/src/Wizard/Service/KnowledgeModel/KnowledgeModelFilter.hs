module Wizard.Service.KnowledgeModel.KnowledgeModelFilter where

import Control.Lens ((&), (.~), (?~), (^.), at, view)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete

filterKnowledgeModel :: [U.UUID] -> KnowledgeModel -> KnowledgeModel
filterKnowledgeModel [] km = km
filterKnowledgeModel selectedTagUuids km = removeOrphanUuids . filterTagUuids . filterTags . filterChapters $ km
  where
    filterChapters km = foldl (filterChapter selectedTagUuids) km (km ^. chaptersL)
    filterTags km = km & tagsM .~ M.filterWithKey (\tUuid _ -> tUuid `elem` selectedTagUuids) (km ^. tagsM)
    filterTagUuids km = km & tagUuids .~ filter (`elem` selectedTagUuids) (km ^. tagUuids)

filterChapter :: [U.UUID] -> KnowledgeModel -> Chapter -> KnowledgeModel
filterChapter selectedTagUuids km chapter =
  let qs = getQuestionsForChapterUuid km (chapter ^. uuid)
   in foldl (filterQuestion selectedTagUuids) km qs

filterQuestion :: [U.UUID] -> KnowledgeModel -> Question -> KnowledgeModel
filterQuestion selectedTagUuids km q =
  case filter (`elem` selectedTagUuids) (q ^. tagUuids') of
    [] -> deleteQuestion km (q ^. uuid')
    filteredQuestionTagUuids -> applyToQuestion filteredQuestionTagUuids q . applyToChildren q $ km
  where
    applyToQuestion filteredQuestionTagUuids q km =
      let updatedQuestion = q & tagUuids' .~ filteredQuestionTagUuids
       in km & questionsM . at (q ^. uuid') ?~ updatedQuestion
    applyToChildren (OptionsQuestion' q) km = foldl go km (q ^. answerUuids)
      where
        go km ansUuid =
          let qs = getQuestionsForAnswerUuid km ansUuid
           in foldl (filterQuestion selectedTagUuids) km qs
    applyToChildren (ListQuestion' q) km =
      let qs = getItemTemplateQuestionsForQuestionUuid km (q ^. uuid)
       in foldl (filterQuestion selectedTagUuids) km qs
    applyToChildren q km = km

removeOrphanUuids :: KnowledgeModel -> KnowledgeModel
removeOrphanUuids = removeOrphanUuidInQuestions . removeOrphanUuidInChapters
  where
    removeOrphanUuidInChapters km = foldl removeOrphanUuidInChapter km (km ^. chaptersL)
    removeOrphanUuidInQuestions km = foldl removeOrphanUuidInQuestion km (km ^. questionsL)

removeOrphanUuidInChapter :: KnowledgeModel -> Chapter -> KnowledgeModel
removeOrphanUuidInChapter km ch = km & chaptersM . at (ch ^. uuid) ?~ (ch & questionUuids .~ newQuestionUuids)
  where
    newQuestionUuids =
      fmap (view uuid') . mapMaybe (\qUuid -> M.lookup qUuid (km ^. questionsM)) $ (ch ^. questionUuids)

removeOrphanUuidInQuestion :: KnowledgeModel -> Question -> KnowledgeModel
removeOrphanUuidInQuestion km q = km & questionsM . at (q ^. uuid') ?~ applyToChildren q
  where
    applyToChildren (OptionsQuestion' q) = OptionsQuestion' $ q & answerUuids .~ newAnswerUuids
      where
        newAnswerUuids =
          fmap (view uuid') . mapMaybe (\ansUuid -> M.lookup ansUuid (km ^. answersM)) $ (q ^. answerUuids)
    applyToChildren (ListQuestion' q) = ListQuestion' $ q & itemTemplateQuestionUuids .~ newQuestionUuids
      where
        newQuestionUuids =
          fmap (view uuid') . mapMaybe (\qUuid -> M.lookup qUuid (km ^. questionsM)) $ (q ^. itemTemplateQuestionUuids)
    applyToChildren q = q
