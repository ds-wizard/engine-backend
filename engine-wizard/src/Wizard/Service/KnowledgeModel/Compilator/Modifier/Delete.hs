module Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete where

import Control.Lens
import qualified Data.Map as M
import qualified Data.UUID as U
import Prelude hiding (lookup)

import LensesConfig
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
  case M.lookup uuid (km ^. chaptersM) of
    Just entity -> deleteNode . deleteChildren entity $ km
    Nothing -> km
  where
    deleteNode km = km & chaptersM .~ M.delete uuid (km ^. chaptersM)
    deleteChildren entity km = foldl deleteQuestion km (entity ^. questionUuids)

deleteQuestion :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteQuestion km uuid =
  case M.lookup uuid (km ^. questionsM) of
    Just entity -> deleteNode . deleteChildren entity $ km
    Nothing -> km
  where
    deleteNode km = km & questionsM .~ M.delete uuid (km ^. questionsM)
    deleteChildren entity = deleteItemTemplateQuestions . deleteAnswers . deleteReferences . deleteExperts
      where
        deleteExperts km = foldl deleteExpert km (entity ^. expertUuids')
        deleteReferences km = foldl deleteReference km (entity ^. referenceUuids')
        deleteAnswers km = foldl deleteAnswer km (entity ^. answerUuids')
        deleteItemTemplateQuestions km = foldl deleteQuestion km (entity ^. itemTemplateQuestionUuids')

deleteExpert :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteExpert km uuid =
  case M.lookup uuid (km ^. expertsM) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = km & expertsM .~ M.delete uuid (km ^. expertsM)

deleteReference :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteReference km uuid =
  case M.lookup uuid (km ^. referencesM) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = km & referencesM .~ M.delete uuid (km ^. referencesM)

deleteAnswer :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteAnswer km uuid =
  case M.lookup uuid (km ^. answersM) of
    Just entity -> deleteNode . deleteChildren entity $ km
    Nothing -> km
  where
    deleteNode km = km & answersM .~ M.delete uuid (km ^. answersM)
    deleteChildren entity km = foldl deleteQuestion km (entity ^. followUpUuids)

deleteChoice :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteChoice km uuid =
  case M.lookup uuid (km ^. choicesM) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = km & choicesM .~ M.delete uuid (km ^. choicesM)

deleteMetric :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteMetric km uuid =
  case M.lookup uuid (km ^. metricsM) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = km & metricsM .~ M.delete uuid (km ^. metricsM)

deletePhase :: KnowledgeModel -> U.UUID -> KnowledgeModel
deletePhase km uuid =
  case M.lookup uuid (km ^. phasesM) of
    Just entity -> deleteNode km
    Nothing -> km
  where
    deleteNode km = km & phasesM .~ M.delete uuid (km ^. phasesM)
