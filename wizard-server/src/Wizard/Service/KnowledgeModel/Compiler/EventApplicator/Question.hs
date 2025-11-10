module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Question where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
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

instance ApplyEvent AddQuestionEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference :: KnowledgeModel -> KnowledgeModel
      addEntityReference km =
        case M.lookup event.parentUuid (getChaptersM km) of
          Just parent ->
            putInChaptersM event.parentUuid (parent {questionUuids = parent.questionUuids ++ [event.entityUuid]}) km
          Nothing ->
            case M.lookup event.parentUuid (getQuestionsM km) of
              Just parent ->
                putInQuestionsM event.parentUuid (setItemTemplateQuestionUuids parent (getItemTemplateQuestionUuids parent ++ [event.entityUuid])) km
              Nothing ->
                case M.lookup event.parentUuid (getAnswersM km) of
                  Just parent ->
                    putInAnswersM event.parentUuid (parent {followUpUuids = parent.followUpUuids ++ [event.entityUuid]}) km
                  Nothing -> km
      addEntity :: KnowledgeModel -> KnowledgeModel
      addEntity = putInQuestionsM event.entityUuid (createEntity event content)

instance ApplyEvent EditQuestionEvent where
  apply = applyEditEvent getQuestionsM setQuestionsM

instance ApplyEvent DeleteQuestionEvent where
  apply event content = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference :: KnowledgeModel -> KnowledgeModel
      deleteEntityReference km =
        case M.lookup event.parentUuid (getChaptersM km) of
          Just parent ->
            putInChaptersM event.parentUuid (parent {questionUuids = L.delete event.entityUuid parent.questionUuids}) km
          Nothing ->
            case M.lookup event.parentUuid (getQuestionsM km) of
              Just parent ->
                putInQuestionsM event.parentUuid (setItemTemplateQuestionUuids parent (L.delete event.entityUuid (getItemTemplateQuestionUuids parent))) km
              Nothing ->
                case M.lookup event.parentUuid (getAnswersM km) of
                  Just parent -> putInAnswersM event.parentUuid (parent {followUpUuids = L.delete event.entityUuid parent.followUpUuids}) km
                  Nothing -> km
      deleteEntity :: KnowledgeModel -> KnowledgeModel
      deleteEntity km = deleteQuestion km event.entityUuid
