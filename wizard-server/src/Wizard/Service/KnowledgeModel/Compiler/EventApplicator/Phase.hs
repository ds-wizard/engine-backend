module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Phase where

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
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Question
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Phase.PhaseEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
