module Wizard.Service.KnowledgeModel.KnowledgeModelFilter where

import Control.Lens ((&), (.~), (?~), (^.), at, view)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.LensesConfig
import Wizard.Model.KnowledgeModel.KnowledgeModelAccessors
import Wizard.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete

filterKnowledgeModel :: [U.UUID] -> KnowledgeModel -> KnowledgeModel
filterKnowledgeModel [] km = km
filterKnowledgeModel selectedTagUuids km = removeOrphanUuids . filterTags . filterChapters $ km
  where
    filterChapters km = foldl (filterChapter selectedTagUuids) km (km ^. chaptersL)
    filterTags km = km & tagsM .~ (M.filterWithKey (\tUuid _ -> tUuid `elem` selectedTagUuids) (km ^. tagsM))

filterChapter :: [U.UUID] -> KnowledgeModel -> Chapter -> KnowledgeModel
filterChapter selectedTagUuids km chapter =
  let qs = getQuestionsForChapterUuid km (chapter ^. uuid)
   in foldl (filterQuestion selectedTagUuids) km qs

filterQuestion :: [U.UUID] -> KnowledgeModel -> Question -> KnowledgeModel
filterQuestion selectedTagUuids km q =
  case filter (\qTagUuid -> qTagUuid `elem` selectedTagUuids) (q ^. tagUuids') of
    [] -> deleteQuestion km (q ^. uuid')
    filteredQuestionTagUuids -> applyToChildren q
  where
    applyToChildren (OptionsQuestion' q) = foldl go km (q ^. answerUuids)
      where
        go km ansUuid =
          let qs = getQuestionsForAnswerUuid km ansUuid
           in foldl (filterQuestion selectedTagUuids) km qs
    applyToChildren (ListQuestion' q) =
      let qs = getItemTemplateQuestionsForQuestionUuid km (q ^. uuid)
       in foldl (filterQuestion selectedTagUuids) km qs
    applyToChildren q = km

removeOrphanUuids :: KnowledgeModel -> KnowledgeModel
removeOrphanUuids = removeOrphanUuidInQuestions . removeOrphanUuidInChapters
  where
    removeOrphanUuidInChapters km = foldl removeOrphanUuidInChapter km (km ^. chaptersL)
    removeOrphanUuidInQuestions km = foldl removeOrphanUuidInQuestion km (km ^. questionsL)

removeOrphanUuidInChapter :: KnowledgeModel -> Chapter -> KnowledgeModel
removeOrphanUuidInChapter km ch = km & chaptersM . at (ch ^. uuid) ?~ (ch & questionUuids .~ newQuestionUuids)
  where
    newQuestionUuids =
      fmap (view uuid') . catMaybes . fmap (\qUuid -> M.lookup qUuid (km ^. questionsM)) $ (ch ^. questionUuids)

removeOrphanUuidInQuestion :: KnowledgeModel -> Question -> KnowledgeModel
removeOrphanUuidInQuestion km q = km & questionsM . at (q ^. uuid') ?~ (applyToChildren q)
  where
    applyToChildren (OptionsQuestion' q) = OptionsQuestion' $ q & answerUuids .~ newAnswerUuids
      where
        newAnswerUuids =
          fmap (view uuid') . catMaybes . fmap (\ansUuid -> M.lookup ansUuid (km ^. answersM)) $ (q ^. answerUuids)
    applyToChildren (ListQuestion' q) = ListQuestion' $ q & itemTemplateQuestionUuids .~ newQuestionUuids
      where
        newQuestionUuids =
          fmap (view uuid') . catMaybes . fmap (\qUuid -> M.lookup qUuid (km ^. questionsM)) $
          (q ^. itemTemplateQuestionUuids)
    applyToChildren q = q
