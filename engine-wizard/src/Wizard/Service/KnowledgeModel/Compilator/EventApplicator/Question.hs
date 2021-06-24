module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Question where

import Control.Lens
import qualified Data.Map as M
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.EventLenses
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
import Wizard.Util.Lens

instance ApplyEvent AddQuestionEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference :: KnowledgeModel -> KnowledgeModel
      addEntityReference km =
        case M.lookup (event ^. parentUuid') (km ^. chaptersM) of
          Just parent ->
            km & (chaptersM . at (event ^. parentUuid')) ?~ (parent & ap questionUuids .~ (event ^. entityUuid'))
          Nothing ->
            case M.lookup (event ^. parentUuid') (km ^. questionsM) of
              Just parent ->
                km & (questionsM . at (event ^. parentUuid')) ?~
                (parent & ap itemTemplateQuestionUuids' .~ (event ^. entityUuid'))
              Nothing ->
                case M.lookup (event ^. parentUuid') (km ^. answersM) of
                  Just parent ->
                    km & (answersM . at (event ^. parentUuid')) ?~ (parent & ap followUpUuids .~ (event ^. entityUuid'))
                  Nothing -> km
      addEntity :: KnowledgeModel -> KnowledgeModel
      addEntity km = km & questionsM . at (event ^. entityUuid') ?~ createEntity event

instance ApplyEvent EditQuestionEvent where
  apply = applyEditEvent (entities . questions) "Question"

instance ApplyEvent DeleteQuestionEvent where
  apply event = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference :: KnowledgeModel -> KnowledgeModel
      deleteEntityReference km =
        case M.lookup (event ^. parentUuid') (km ^. chaptersM) of
          Just parent ->
            km & (chaptersM . at (event ^. parentUuid')) ?~ (parent & del questionUuids .~ (event ^. entityUuid'))
          Nothing ->
            case M.lookup (event ^. parentUuid') (km ^. questionsM) of
              Just parent ->
                km & (questionsM . at (event ^. parentUuid')) ?~
                (parent & del itemTemplateQuestionUuids' .~ (event ^. entityUuid'))
              Nothing ->
                case M.lookup (event ^. parentUuid') (km ^. answersM) of
                  Just parent ->
                    km & (answersM . at (event ^. parentUuid')) ?~
                    (parent & del followUpUuids .~ (event ^. entityUuid'))
                  Nothing -> km
      deleteEntity :: KnowledgeModel -> KnowledgeModel
      deleteEntity km = deleteQuestion km (event ^. entityUuid')
