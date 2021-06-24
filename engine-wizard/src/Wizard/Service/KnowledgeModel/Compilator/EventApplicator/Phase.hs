module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Phase where

import Control.Lens
import qualified Data.Map as M
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.EventLenses
import Shared.Model.Event.Phase.PhaseEvent
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
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
import Wizard.Util.Lens

instance ApplyEvent AddPhaseEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km & ap phaseUuids .~ (event ^. entityUuid')
      addEntity km = km & phasesM . at (event ^. entityUuid') ?~ createEntity event

instance ApplyEvent EditPhaseEvent where
  apply = applyEditEvent (entities . phases) "Phase"

instance ApplyEvent DeletePhaseEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km & del phaseUuids .~ (event ^. entityUuid')
      deleteEntity km = deletePhase km (event ^. entityUuid')
      deleteEntityChildrenReference km =
        km & entities . questions .~ M.map (deletePhaseReference event) (km ^. entities . questions)
