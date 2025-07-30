module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Question where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance ApplyEvent AddQuestionEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference :: KnowledgeModel -> KnowledgeModel
      addEntityReference km =
        case M.lookup (getParentUuid event) (getChaptersM km) of
          Just parent ->
            putInChaptersM (getParentUuid event) (parent {questionUuids = parent.questionUuids ++ [getEntityUuid event]}) km
          Nothing ->
            case M.lookup (getParentUuid event) (getQuestionsM km) of
              Just parent ->
                putInQuestionsM (getParentUuid event) (setItemTemplateQuestionUuids parent (getItemTemplateQuestionUuids parent ++ [getEntityUuid event])) km
              Nothing ->
                case M.lookup (getParentUuid event) (getAnswersM km) of
                  Just parent ->
                    putInAnswersM (getParentUuid event) (parent {followUpUuids = parent.followUpUuids ++ [getEntityUuid event]}) km
                  Nothing -> km
      addEntity :: KnowledgeModel -> KnowledgeModel
      addEntity = putInQuestionsM (getEntityUuid event) (createEntity event)

instance ApplyEvent EditQuestionEvent where
  apply = applyEditEvent getQuestionsM setQuestionsM

instance ApplyEvent DeleteQuestionEvent where
  apply event = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference :: KnowledgeModel -> KnowledgeModel
      deleteEntityReference km =
        case M.lookup (getParentUuid event) (getChaptersM km) of
          Just parent ->
            putInChaptersM (getParentUuid event) (parent {questionUuids = L.delete (getEntityUuid event) parent.questionUuids}) km
          Nothing ->
            case M.lookup (getParentUuid event) (getQuestionsM km) of
              Just parent ->
                putInQuestionsM (getParentUuid event) (setItemTemplateQuestionUuids parent (L.delete (getEntityUuid event) (getItemTemplateQuestionUuids parent))) km
              Nothing ->
                case M.lookup (getParentUuid event) (getAnswersM km) of
                  Just parent -> putInAnswersM (getParentUuid event) (parent {followUpUuids = L.delete (getEntityUuid event) parent.followUpUuids}) km
                  Nothing -> km
      deleteEntity :: KnowledgeModel -> KnowledgeModel
      deleteEntity km = deleteQuestion km (getEntityUuid event)
