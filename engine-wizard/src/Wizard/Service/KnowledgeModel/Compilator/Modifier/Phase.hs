module Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Phase.PhaseEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddPhaseEvent Phase where
  createEntity e = Phase {_phaseUuid = e ^. entityUuid, _phaseTitle = e ^. title, _phaseDescription = e ^. description}

instance EditEntity EditPhaseEvent Phase where
  editEntity e = applyDescription . applyTitle
    where
      applyTitle exp = applyValue (e ^. title) exp title
      applyDescription exp = applyValue (e ^. description) exp description
