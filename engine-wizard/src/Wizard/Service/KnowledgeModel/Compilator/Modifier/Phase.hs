module Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Phase.PhaseEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddPhaseEvent Phase where
  createEntity e =
    Phase
      { _phaseUuid = e ^. entityUuid
      , _phaseTitle = e ^. title
      , _phaseDescription = e ^. description
      , _phaseAnnotations = e ^. annotations
      }

instance EditEntity EditPhaseEvent Phase where
  editEntity e = applyAnnotations . applyDescription . applyTitle
    where
      applyTitle p = applyValue (e ^. title) p title
      applyDescription p = applyValue (e ^. description) p description
      applyAnnotations p = applyValue (e ^. annotations) p annotations
