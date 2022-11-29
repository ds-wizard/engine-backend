module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Phase where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.Event.EventLenses
import Shared.Model.Event.Phase.PhaseEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
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
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddPhaseEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {phaseUuids = km.phaseUuids ++ [getEntityUuid event]}
      addEntity = putInPhasesM (getEntityUuid event) (createEntity event)

instance ApplyEvent EditPhaseEvent where
  apply = applyEditEvent getPhasesM setPhasesM

instance ApplyEvent DeletePhaseEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {phaseUuids = L.delete (getEntityUuid event) km.phaseUuids}
      deleteEntity km = deletePhase km (getEntityUuid event)
      deleteEntityChildrenReference km =
        setQuestionsM km $ M.map (deletePhaseReference event) km.entities.questions
