module Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Prelude hiding (lookup)

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()

deleteChapter :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteChapter km uuid =
  case M.lookup uuid (getChaptersM km) of
    Just entity -> deleteNode . deleteChildren entity $ km
    Nothing -> km
  where
    deleteNode km = setChaptersM km (M.delete uuid (getChaptersM km))
    deleteChildren entity km = foldl deleteQuestion km entity.questionUuids

deleteQuestion :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteQuestion km uuid =
  case M.lookup uuid (getQuestionsM km) of
    Just entity -> deleteNode . deleteChildren entity $ km
    Nothing -> km
  where
    deleteNode km = setQuestionsM km (M.delete uuid (getQuestionsM km))
    deleteChildren :: Question -> KnowledgeModel -> KnowledgeModel
    deleteChildren entity = deleteItemTemplateQuestions . deleteAnswers . deleteReferences . deleteExperts
      where
        deleteExperts km = foldl deleteExpert km (getExpertUuids entity)
        deleteReferences km = foldl deleteReference km (getReferenceUuids entity)
        deleteAnswers km = foldl deleteAnswer km (getAnswerUuids entity)
        deleteItemTemplateQuestions km = foldl deleteQuestion km (getItemTemplateQuestionUuids entity)

deleteExpert :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteExpert km uuid =
  case M.lookup uuid (getExpertsM km) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = setExpertsM km (M.delete uuid (getExpertsM km))

deleteReference :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteReference km uuid =
  case M.lookup uuid (getReferencesM km) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = setReferencesM km (M.delete uuid (getReferencesM km))

deleteAnswer :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteAnswer km uuid =
  case M.lookup uuid (getAnswersM km) of
    Just entity -> deleteNode . deleteChildren entity $ km
    Nothing -> km
  where
    deleteNode km = setAnswersM km (M.delete uuid (getAnswersM km))
    deleteChildren entity km = foldl deleteQuestion km entity.followUpUuids

deleteChoice :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteChoice km uuid =
  case M.lookup uuid (getChoicesM km) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = setChoicesM km (M.delete uuid (getChoicesM km))

deleteMetric :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteMetric km uuid =
  case M.lookup uuid (getMetricsM km) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = setMetricsM km (M.delete uuid (getMetricsM km))

deletePhase :: KnowledgeModel -> U.UUID -> KnowledgeModel
deletePhase km uuid =
  case M.lookup uuid (getPhasesM km) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = setPhasesM km (M.delete uuid (getPhasesM km))
