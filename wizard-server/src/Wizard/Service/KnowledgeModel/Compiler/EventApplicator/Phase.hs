module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Phase where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Phase.PhaseEvent
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
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Question
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddPhaseEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {phaseUuids = km.phaseUuids ++ [event.entityUuid]}
      addEntity = putInPhasesM event.entityUuid (createEntity event content)

instance ApplyEvent EditPhaseEvent where
  apply = applyEditEvent getPhasesM setPhasesM

instance ApplyEvent DeletePhaseEvent where
  apply event content = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {phaseUuids = L.delete event.entityUuid km.phaseUuids}
      deleteEntity km = deletePhase km event.entityUuid
      deleteEntityChildrenReference km =
        setQuestionsM km $ M.map (deletePhaseReference event) km.entities.questions
